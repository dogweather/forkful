---
title:                "Ein Datum aus einem String auslesen"
html_title:           "Lua: Ein Datum aus einem String auslesen"
simple_title:         "Ein Datum aus einem String auslesen"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?
Das Parsen eines Datums aus einem String ist der Prozess, bei dem ein Datum in einem bestimmten Format aus einem Text extrahiert wird. Programmierer tun dies, um Daten, die als Text vorliegen, in ein verarbeitbares Format umzuwandeln.

## Wie geht's?
Das Datum kann mithilfe von Lua's `os.date()` Funktion geparst werden. Zum Beispiel:

```Lua
local date_string = "12.03.2021"
local parsed_date = os.date("%d.%m.%Y", date_string)

print(parsed_date) -- Output: 12.03.2021
```

Das erste Argument der `os.date()` Funktion ist das gewünschte Format des Datums, das zweite Argument ist der Textstring aus dem das Datum geparst werden soll. In diesem Beispiel verwenden wir das Format `"%d.%m.%Y"`, um das Datum in dem in Deutschland üblichen Format "Tag.Monat.Jahr" auszugeben.

Um das Datum in verschiedene andere Formate zu parsen, können verschiedene Formatierungszeichen verwendet werden. Eine vollständige Liste aller Formatierungszeichen und deren Bedeutung kann in Lua's offizieller Dokumentation gefunden werden.

## Tiefere Einblicke
Das Parsen von Daten aus Strings ist ein gängiger Prozess in der Programmierung, da Daten oft als Text vorliegen und in ein strukturiertes Format umgewandelt werden müssen. Es gibt auch alternative Möglichkeiten, ein Datum aus einem String zu parsen, zum Beispiel mit regulären Ausdrücken oder durch den Gebrauch von Bibliotheken.

Um Datumsangaben aus anderen Ländern oder Regionen zu parsen, müssen oft unterschiedliche Formatierungszeichen verwendet werden, um das Datum korrekt darzustellen. Einige Programmiersprachen, wie Java oder Python, bieten eingebaute Funktionen für die Datumsverarbeitung mit mehr Flexibilität bei der Formatierung.

## Siehe auch
- [Lua Referenzhandbuch](https://www.lua.org/manual/5.4/manual.html#6.9)
- [Date Parsing in anderen Programmiersprachen](https://en.wikipedia.org/wiki/Date_parsing_in_different_programming_languages)