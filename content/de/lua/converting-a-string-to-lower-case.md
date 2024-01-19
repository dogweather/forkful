---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmierung wird die Umwandlung eines Strings in Kleinbuchstaben verwendet, um die Groß- und Kleinschreibung irrelevant zu machen, z. B. in Suchanfragen. Es erleichtert auch die Dateneingabe und -verarbeitung, indem Konsistenz gewährleistet wird.

## Wie geht das:

Die Stringbibliothek in Lua bietet eine integrierte Funktion `string.lower()` zum Umwandeln von Zeichenketten in Kleinbuchstaben. Hier ist ein Beispielcode:

```Lua
str = "Hallo Welt!"
lowercase_str = string.lower(str)
print(lowercase_str)
```

Und die Ausgabe wäre:

```Lua
hallo welt!
```

## Deep Dive

- Historischer Kontext: Die Fähigkeit, einen String in Kleinbuchstaben umzuwandeln, ist eine Kernfunktionalität, die fast in jeder Programmiersprache existiert, darunter natürlich auch in Lua. Es ist einer der ersten Schritte zum Manipulieren von Textdaten in beliebigen Anwendungen.

- Alternativen: In Lua gibt es keine eingebaute Alternative zu `string.lower()`. Wenn jedoch aus irgendeinem Grund `string.lower()` nicht verwendet werden kann, könnte man selbst eine Funktion schreiben, die durch jeden Buchstaben des Strings iteriert und ihn dann manuell in einen Kleinbuchstaben umwandelt.

- Implementierungsdetails: `string.lower()` funktioniert, indem für jedes Zeichen des Strings geprüft wird, ob es sich um einen Großbuchstaben handelt. Wenn ja, ersetzt es diesen durch den entsprechenden Kleinbuchstaben.

## Siehe auch

- Lua String Bibliothek Dokumentation: https://www.lua.org/manual/5.4/manual.html#6.4

- Alternative Methode, einen String in Lua in Kleinbuchstaben zu konvertieren: https://rosettacode.org/wiki/Letter_frequency#Lua

Bitte beachten Sie, dass der obige alternative Code nur zu Lern- und Vergleichszwecken und nicht für die Produktion vorgesehen ist. Immer das eingebaute `string.lower()` für Produktionszwecke verwenden.