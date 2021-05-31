;;; swedish-holidays.el --- Set ‘holiday-local-holidays’ for Sweden  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Anders Johansson

;; Author: Anders Johansson
;; Keywords: calendar, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Set ‘holiday-local-holidays’ to the list of “allmäna helgdagar” and
;; ”helgdagsaftnar” from https://sv.wikipedia.org/wiki/Helgdagar_i_Sverige

;;; Code:

(setq holiday-local-holidays
      '((holiday-fixed 1 1 "Nyårsdagen")
        (holiday-fixed 1 5 "Trettondagsafton")
        (holiday-fixed 1 6 "Trettondedag jul")
        (holiday-easter-etc -3 "Skärtorsdagen")
        (holiday-easter-etc -2 "Långfredagen")
        (holiday-easter-etc -1 "Påskafton")
        (holiday-easter-etc 0 "Påskdagen")
        (holiday-easter-etc 1 "Annandag påsk")
        (holiday-fixed 4 30 "Valborgsmässoafton")
        (holiday-fixed 5 1 "Första maj")
        (holiday-easter-etc 39 "Kristi himmelsfärdsdag") ;; sjätte torsdagen efter påskdagen
        (holiday-easter-etc 48 "Pingstafton")
        (holiday-easter-etc 49 "Pingstdagen") ;; sjunde söndagen efter påskdagen
        (holiday-fixed 6 6 "Sveriges nationaldag")
        (holiday-float 6 5 1 "Midsommarafton" 19) ;; fredagen mellan 19 juni och 25 juni
        (holiday-float 6 6 1 "Midsommardagen" 20) ;; lördagen mellan 20 juni och 26 juni
        (holiday-float 10 5 1 "Allhelgonaafton" 30) ;; fredag mellan 30 oktober och 5 november
        (holiday-float 10 6 1 "Alla helgons dag" 31) ;; lördagen som infaller under perioden från 31 oktober till 6 november
        (holiday-fixed 12 24 "Julafton")
        (holiday-fixed 12 25 "Juldagen")
        (holiday-fixed 12 26 "Annandag jul")
        (holiday-fixed 12 31 "Nyårsafton")))

(provide 'swedish-holidays)
;;; swedish-holidays.el ends here
