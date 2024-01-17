---
title:                "Unterschneidungen extrahieren"
html_title:           "Haskell: Unterschneidungen extrahieren"
simple_title:         "Unterschneidungen extrahieren"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Extrahieren von Teilstrings, auch bekannt als Teilausdrücke, ist ein häufig verwendeter Prozess in der Programmierung. Es ermöglicht Programmierern, bestimmte Teile eines Texts zu isolieren und für verschiedene Zwecke zu verwenden. Zum Beispiel kann das Extrahieren von Teilstrings nützlich sein, um bestimmte Wörter oder Sätze in einer Zeichenfolge zu suchen oder um Daten zu formatieren.

# Wie geht das?

Die Extraktion von Teilstrings in Haskell ist sehr einfach und kann mit Hilfe der Funktion `take` und `drop` durchgeführt werden. Diese beiden Funktionen ermöglichen es uns, die ersten n Zeichen einer Zeichenfolge oder den Rest einer Zeichenfolge ab dem n-ten Zeichen zu extrahieren. Hier ist ein Beispielcode:

```Haskell
-- Extrahiere die ersten 5 Zeichen
take 5 "Hallo Welt!"  --> "Hallo"

-- Extrahiere den Rest der Zeichenfolge ab dem 6. Zeichen
drop 6 "Hallo Welt!"  --> "Welt!"
```

In diesem Beispiel werden die Funktionen `take` und `drop` auf eine Zeichenfolge angewendet, um Teilstrings zu extrahieren. Das erste Argument ist immer die Anzahl der zu extrahierenden Zeichen, gefolgt von der Zeichenfolge selbst.

# Tieferer Einblick

Die Extraktion von Teilstrings ist ein grundlegender Prozess in der Programmierung und wird in verschiedenen Anwendungsbereichen häufig verwendet. Eines der häufigsten Anwendungsgebiete ist die Verarbeitung von Benutzereingaben in Formularen oder das Lesen und Analysieren von Dateien.

Alternativ können Programmierer auch reguläre Ausdrücke verwenden, um bestimmte Muster in einer Zeichenfolge zu suchen und Teilstrings darauf basierend zu extrahieren.

In Haskell erfolgt die Implementierung der Funktionen `take` und `drop` auf der Basis der sogenannten "List Comprehensions". Dabei handelt es sich um eine spezielle Syntax zur Erstellung von Listen aus anderen Listen. Dies ermöglicht ein einfaches und elegantes Konzept zur Extraktion von Teilstrings.

# Weitere Quellen

- Dokumentation der `take` und `drop` Funktionen in der offiziellen Haskell-Dokumentation. 
(http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#g:35)

- Eine Einführung in die Verwendung von regulären Ausdrücken in Haskell: (https://wiki.haskell.org/Regular_expressions)

- Eine tiefergehende Analyse der Implementierung von Teilstrings in Haskell: (https://chrisdone.com/posts/haskell-string-search/)