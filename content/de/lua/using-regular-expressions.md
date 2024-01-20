---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Lua: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was ist das und warum? 
Wenn du schon einmal an einem komplizierten Dokument gearbeitet hast und nach bestimmten Textfragmenten suchen musstest, weißt du, wie mühsam das sein kann. Hier kommen reguläre Ausdrücke (regular expressions) ins Spiel. Es ist eine Art von spezieller Schreibweise, die es uns ermöglicht, nach bestimmten Mustern in einem Text zu suchen und sie zu manipulieren. Programmierer nutzen reguläre Ausdrücke, um schneller und effizienter Daten zu verarbeiten.

## So geht's:
Ein Beispiel: Um in einem Text nach allen Telefonnummern zu suchen, die im Format xxx-xxx-xxxx geschrieben sind, könnten wir folgenden regulären Ausdruck verwenden: `(\d{3})-(\d{3})-(\d{4})`. Im Code würde es so aussehen:

```Lua
text = "Meine Telefonnummer ist 123-456-7890 und die meines Freundes ist 987-654-3210"
pattern = "(\d{3})-(\d{3})-(d{4})"
numbers = {}

for a, b, c in text:gmatch(pattern) do
  table.insert(numbers, a)
  table.insert(numbers, b)
  table.insert(numbers, c)
end

print("Gefundene Nummern: ")
for i, number in ipairs(numbers) do
  print(number)
end

```

Die Ausgabe wäre dann:
```
123
456
7890
987
654
3210
```

## Tiefes Eintauchen:
Reguläre Ausdrücke wurden in den 1950er Jahren von einem Mathematiker entwickelt und ursprünglich von Texteditoren verwendet. Heutzutage werden sie von vielen Programmiersprachen unterstützt, einschließlich Lua. Es gibt auch Alternative Methoden zur Textmanipulation, wie z.B. das Verwenden von Mustern und das Splitten von Textstrings. Die Implementierung von regulären Ausdrücken in Lua basiert auf dem sogenannten "Pattern Matching", das auf Algorithmen der formalen Spracherkennung beruht.

## Sieh dir auch an:
- [Tutorial zur Verwendung von regulären Ausdrücken in Lua](https://www.tutorialspoint.com/pattern-matching-in-lua)
- [Lua regelbasierte Ausdrücke reference manual](https://www.lua.org/manual/5.3/manual.html#6.4.1)