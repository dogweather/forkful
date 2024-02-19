---
aliases:
- /de/clojure/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:48.040040-07:00
description: "Das Schreiben auf den Standardfehler (stderr) bezieht sich darauf, Fehlermeldungen\
  \ und Diagnosen an den stderr-Stream zu leiten, getrennt von der\u2026"
lastmod: 2024-02-18 23:09:04.524501
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (stderr) bezieht sich darauf, Fehlermeldungen\
  \ und Diagnosen an den stderr-Stream zu leiten, getrennt von der\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf den Standardfehler (stderr) bezieht sich darauf, Fehlermeldungen und Diagnosen an den stderr-Stream zu leiten, getrennt von der Standardausgabe (stdout). Programmierer tun dies, um die reguläre Programmausgabe von Fehlermeldungen zu unterscheiden, was ein effektiveres Debugging und Protokollieren ermöglicht.

## Wie geht das:
In Clojure können Sie mit dem `*err*` Stream auf stderr schreiben. Hier ist ein einfaches Beispiel:

```clojure
(.write *err* "Dies ist eine Fehlermeldung.\n")
```

Beachten Sie, dass Sie nach dem Verfassen einer Nachricht den Stream flushen sollten, um sicherzustellen, dass die Nachricht sofort ausgegeben wird:

```clojure
(flush)
```

Beispiel für eine Ausgabe auf stderr:
```
Dies ist eine Fehlermeldung.
```

Wenn Sie mit Ausnahmen umgehen, möchten Sie vielleicht Stacktraces auf stderr ausgeben. Verwenden Sie dafür `printStackTrace`:

```clojure
(try
  ;; Code, der eine Ausnahme auslösen könnte
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

Für ein strukturierteres Fehlerprotokollieren können Drittanbieter-Bibliotheken wie `timbre` konfiguriert werden, um auf stderr zu protokollieren. Hier ist eine grundlegende Einrichtung und Verwendung:

Zuerst fügen Sie `timbre` Ihren Abhängigkeiten hinzu. Dann konfigurieren Sie es für die Nutzung von stderr:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; Deaktivieren des stdout-Protokollierens
(timbre/set-config! [:appenders :spit :enabled?] false) ;; Deaktivieren des Dateiprotokollierens
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; Aktivieren von stderr für Fehler

(timbre/error "Ein Fehler ist bei der Bearbeitung Ihrer Anfrage aufgetreten.")
```

Dies wird Fehlermeldungen auf stderr leiten und sie von der Standardanwendungsausgabe unterscheiden.
