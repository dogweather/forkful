---
date: 2024-01-20 17:52:21.343628-07:00
description: ''
lastmod: '2024-04-05T22:00:06.548397-06:00'
model: gpt-4-1106-preview
summary: ''
title: Debug-Ausgaben drucken
weight: 33
---

## Anleitung:


### Einfache Ausgabe mit `println`:
```Clojure
(println "Das ist ein Debug-Wert:" (+ 2 2))
;; Ausgabe: Das ist ein Debug-Wert: 4
```

### Formatierung mit `format`:
```Clojure
(defn debug-output [value]
  (println (format "Debug-Info: %s" value)))

(debug-output "Wichtige Daten")
;; Ausgabe: Debug-Info: Wichtige Daten
```

### Logging mit `tools.logging`:
```Clojure
(require '[clojure.tools.logging :as log])

(log/info "Das ist eine Info-Log-Nachricht")
;; Ausgabe: INFO [namespace] - Das ist eine Info-Log-Nachricht
```

## Hintergrund:
In Clojure ist 'println' die schnellste Methode, um Werte während der Entwicklung auszugeben. Doch für komplexere Anwendungen ist das Logging-System vorzuziehen. Bevor 'println' populär wurde, nutzten Programmierer oft das Schreiben in temporäre Dateien, aber das war umständlich. Die `tools.logging`-Bibliothek ermöglicht ein level-basiertes Logging und ist flexibler als `println`, weil es verschiedene Output-Formate und -Ziele unterstützt und meist bei der Produktion genutzt wird.

## Siehe auch:
- Clojure's `tools.logging` Library: [https://github.com/clojure/tools.logging](https://github.com/clojure/tools.logging)
