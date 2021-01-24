;;; spotify-album-search.el --- Spotify.el album search major mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018 Daniel Fernandes Martins

;;; Commentary:

;; This library implements UI and a major mode for searching and acting on Spotify albums.

;;; Code:

(require 'spotify-api)
(require 'spotify-controller)
(require 'spotify-track-search)

(defvar spotify-current-page)
(defvar spotify-browse-message)
(defvar spotify-query)

(defvar spotify-album-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "M-RET") 'spotify-album-select)
    (define-key map (kbd "l")     'spotify-album-load-more)
    (define-key map (kbd "g")     'spotify-album-reload)
    (define-key map (kbd "f")     'spotify-album-save)
    (define-key map (kbd "u")     'spotify-album-remove)
    map)
  "Local keymap for `spotify-album-search-mode' buffers.")

;; Enables the `spotify-remote-mode' in the album search buffer
(add-hook 'spotify-album-search-mode-hook 'spotify-remote-mode)

(define-derived-mode spotify-album-search-mode tabulated-list-mode "Album-Search"
  "Major mode for displaying the albums returned by a Spotify search.")

(defun spotify-album-select ()
  "Plays the album under the cursor."
  (interactive)
  (let* ((selected-album (tabulated-list-get-id)))
    (spotify-play-track nil selected-album)))

(defun spotify-album-reload ()
  "Reloads the first page of results for the current album view."
  (interactive)
  (let ((page 1))
    (cond ((bound-and-true-p spotify-query)          (spotify-album-search-update spotify-query page))
          (t                                         (spotify-my-albums-update page)))))

(defun spotify-album-load-more ()
  "Load the next page of results for the current album view."
  (interactive)
  (let ((next-page (1+ spotify-current-page)))
    (cond ((bound-and-true-p spotify-query)          (spotify-album-search-update spotify-query next-page))
          (t                                         (spotify-my-albums-update next-page)))))

(defun spotify-album-save ()
  "Save the album under this cursor in the current user's library."
  (interactive)
  (let* ((selected-album (tabulated-list-get-id))
         (name (spotify-get-item-name selected-album)))
    (when (y-or-n-p (format "Save album '%s'? " name))
      (spotify-api-album-save
       selected-album
       (lambda (_)
         (message (format "Saved album '%s'" name)))))))

(defun spotify-album-remove ()
  "Remove the album under this cursor from the current user's library."
  (interactive)
  (let* ((selected-album (tabulated-list-get-id))
         (name (spotify-get-item-name selected-album)))
    (when (y-or-n-p (format "Remove album '%s'? " name))
      (spotify-api-album-remove
       selected-album
       (lambda (&rest args)
         (setq debug-album-args args)
         (message (format "Remove album '%s'" name)))))))

(defun spotify-album-search-update (query page)
  "Fetch the PAGE of results using QUERY at the search endpoint."
  (let ((buffer (current-buffer)))
    (spotify-api-search
     'album
     query
     page
     (lambda (albums)
       (if-let ((items (spotify-get-search-album-items albums)))
           (with-current-buffer buffer
             (setq-local spotify-current-page page)
             (setq-local spotify-query query)
             (pop-to-buffer buffer)
             (spotify-album-search-print items page)
             (message "Album view updated"))
         (message "No more albums"))))))

(defun spotify-my-albums-update (page)
  "Fetch PAGE of results using the album endpoint for the current user."
  (let ((buffer (current-buffer)))
    (spotify-api-my-albums
     page
     (lambda (albums)
       (if-let ((items (spotify-get-items albums)))
           (with-current-buffer buffer
             (setq-local spotify-current-page page)
             (pop-to-buffer buffer)
             (spotify-album-search-print
              (mapcar (lambda (item)
                        (spotify-get-library-item-album item))
                      items)
              page)
             (message "Album view updated"))
         (message "No more albums"))))))

(defun spotify-album-set-list-format ()
  "Configure the column data for the typical album view."
  (setq tabulated-list-format
        (vconcat (vector `("Album Name" ,(- (window-width) 45) t)
                         `("Artist" 30 t)
                         '("# Tracks" 8 (lambda (row-1 row-2)
                                          (< (spotify-get-album-track-count (car row-1))
                                             (spotify-get-album-track-count (car row-2)))) :right-align t)))))

(defun spotify-album-search-print (albums page)
  "Append ALBUMS to PAGE of the current album view."
  (let (entries)
    (dolist (album albums)
      (let ((album-name (spotify-get-item-name album))
            (artist-name (spotify-get-item-name (spotify-get-album-artist-name album))))
        (push (list album
                    (vector 
                     (cons album-name
                           (list 'face 'link
                                 'follow-link t
                                 'action `(lambda (_) (spotify-album-tracks ,album))
                                 'help-echo (format "Show %s's tracks" album-name)
			         'artist-or-album 'album))
                     (cons artist-name
                           (list 'face 'link
                                 'follow-link t
                                 'action `(lambda (_) (spotify-track-search ,(format "artist:\"%s\"" artist-name)))
                                 'help-echo (format "Show %s's tracks" artist-name)
				 'artist-or-album 'artist))
                     (number-to-string (spotify-get-album-track-count album))))
              entries)))
    (spotify-album-set-list-format)
    (when (eq 1 page) (setq-local tabulated-list-entries nil))
    (setq-local tabulated-list-entries (append tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(provide 'spotify-album-search)
;;; spotify-album-search.el ends here
