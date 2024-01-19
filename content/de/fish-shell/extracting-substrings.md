---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Extrahieren von Teilzeichenketten (substrings) ist ein Prozess, bei dem ein bestimmter Abschnitt einer Zeichenkette (string) ausgewählt und getrennt wird. Dies ist in der Programmierung üblich, um Daten zu analysieren, zu manipulieren oder zum Filtern von Eingabedaten.

## Anleitung:

Im Folgenden finden Sie Beispiele in Fish Shell und deren Ausgaben:

```Fish Shell
set string 'Fischschale'
echo $string[1..5]
```
 
Ausgabe:

```Fish Shell
Fisch
```
In diesem Beispiel wird ein Teilstring aus den ersten fünf Buchstaben des strings erstellt.

```Fish Shell
set string 'Fischschale'
echo $string[-5..-1]
```

Ausgabe:

```Fish Shell
Schale
```
Hier wird ein Teilstring aus den letzten fünf Buchstaben des strings erstellt.


## Vertiefung:

Teilzeichenketten ziehen ihre Wurzeln aus den frühesten Tagen der Programmierung ziehen, als Speicherplatz teuer und begrenzt war. Durch die Verwendung von Teilzeichenketten konnte effizienter Code geschrieben werden. Heute werden sie aus den gleichen Gründen und mehr genutzt.

Alternativen zur Teilzeichenkettenextraktion in Fish Shell könnten durch Regular Expressions (Regex) oder das `string`-Befehl in Fish Shell sein. Aber die klare und unkomplizierte Methode des Slicing in Fish Shell macht es zu einer attraktiven Option für viele.

Fish Shell implementiert slicing mit einer 1-basierten Indexierung, das heißt der erste Buchstabe hat den Index 1 und so weiter. Im Gegensatz zu vielen anderen Sprachen erlaubt Fish negative Indizes, die von hinten zählen.

## Siehe auch:

Weitere Informationen zur Zeichenkettenmanipulation in Fish Shell finden Sie in der offiziellen Fish-Dokumentation: https://fishshell.com/docs/current/index.html. Insbesondere der Abschnitt über die `string`-Befehle bietet einen reichen Überblick über diesen Bereich.