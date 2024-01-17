---
title:                "Unterstrings extrahieren"
html_title:           "Python: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum

Das Extrahieren von Teilzeichenketten ist eine häufig verwendete Funktion in der Programmierung. Es ermöglicht uns, einen Teil einer Zeichenkette, die aus mehreren Zeichen besteht, auszuwählen und zu isolieren. Programmierer verwenden dies oft, um spezifische Informationen aus einer größeren Zeichenkette zu extrahieren, um sie z.B. in andere Variablen oder Funktionen zu übergeben.

## Wie:

```python
# Beispiel 1
string = "Hallo, mein Name ist Maria"
print(string[6:10])
# Ausgabe: m
 
# Beispiel 2
satz = "Heute ist ein schöner Tag in Deutschland"
print(satz[27:37])
# Ausgabe: schöner Tag
```

## Tiefer tauchen:

Die Idee, Teilzeichenketten zu extrahieren, stammt ursprünglich aus der Sprache SNOBOL, die in den 1960er Jahren entwickelt wurde. Heutzutage gibt es einige alternative Möglichkeiten, Teilzeichenketten in Python zu extrahieren, wie z.B. die split() -Funktion, die eine Zeichenkette an einem bestimmten Zeichen in mehrere Teilzeichenketten aufteilt. Die Implementierung hinter dem Extrahieren von Teilzeichenketten basiert auf der Verwendung von Indizes oder möglicherweise sogar regulären Ausdrücken, je nachdem, welche Methode verwendet wird.

## Siehe auch:

- [Python-Dokumentation zur String-Indexierung](https://docs.python.org/3/tutorial/introduction.html#strings)
- [Wikipedia-Artikel über SNOBOL](https://en.wikipedia.org/wiki/SNOBOL)
- [Python split() -Funktion](https://www.w3schools.com/python/ref_string_split.asp)