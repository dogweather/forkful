---
date: 2024-01-20 17:45:30.285772-07:00
description: "How to: \"Tiefergehendes\" Das Extrahieren von Substrings findet seit\
  \ den fr\xFChesten Tagen der Programmierung statt. Alternativen in anderen Sprachen\
  \ sind\u2026"
lastmod: '2024-04-05T22:51:08.073890-06:00'
model: gpt-4-1106-preview
summary: "\"Tiefergehendes\" Das Extrahieren von Substrings findet seit den fr\xFC\
  hesten Tagen der Programmierung statt."
title: Teilstrings extrahieren
weight: 6
---

## How to:
"So geht's:"
```clojure
;; Einfaches Extrahieren eines Substrings
(defn extract-substring [s start end]
  (subs s start end))

;; Beispiel:
(println (extract-substring "Hallo, Welt!" 7 12)) ; Gibt "Welt" aus
```

```clojure
;; Extrahieren mit negativen Indices (von hinten gezählt)
(defn extract-substring-from-end [s end]
  (subs s (- (count s) end) (count s)))

;; Beispiel:
(println (extract-substring-from-end "Programmieren macht Spaß" 4)) ; Gibt "Spaß" aus
```

## Deep Dive
"Tiefergehendes"
Das Extrahieren von Substrings findet seit den frühesten Tagen der Programmierung statt. Alternativen in anderen Sprachen sind Methoden wie `substring()`, `slice()` oder sogar reguläre Ausdrücke. Clojure verwendet `subs`, welche direkt auf Java-Strings operiert und daher sehr schnell ist. Beachte jedoch, dass `subs` eine IllegalArgumentException wirft, wenn die Indices ungültig sind.

## See Also
"Weiterführende Links"
- ClojureDocs zu `subs`: https://clojuredocs.org/clojure.core/subs
- Das Clojure Style Guide: https://guide.clojure.style/
- Java String Dokumentation (wichtig für Clojure): https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
