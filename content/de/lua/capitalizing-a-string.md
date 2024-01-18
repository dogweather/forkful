---
title:                "Eine Zeichenfolge großschreiben"
html_title:           "Lua: Eine Zeichenfolge großschreiben"
simple_title:         "Eine Zeichenfolge großschreiben"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Kapitalisieren einer Zeichenfolge bedeutet, dass der Anfangsbuchstabe jeder einzelnen Wortes in der Zeichenfolge großgeschrieben wird. Programmierer tun dies oft, um die Lesbarkeit von Code zu verbessern und die Bedeutung von Variablen und Funktionen hervorzuheben.

## Wie geht's?

Eine Zeichenfolge in Lua kann ganz einfach mit der Funktion `string.upper()` kapitalisiert werden. Schauen wir uns das folgende Beispiel an:

```Lua
local string = "hallo welt"
local capitalString = string.upper(string)
print(capitalString)
```

Das Ergebnis des obigen Codes ist `HALLO WELT`, da alle Buchstaben in der Zeichenfolge großgeschrieben wurden. Dieser Vorgang kann auch in einer einzigen Zeile durchgeführt werden, indem man die Funktion direkt auf die Zeichenfolge anwendet:

```Lua
print(("hallo welt"):upper())
```

## Tiefes Eintauchen

Das Kapitalisieren von Zeichenfolgen ist keine spezifische Lua-Funktion, sondern eine generelle Programmierkonvention. Es wird oft verwendet, um Variablen, Funktionen oder andere Codeelemente hervorzuheben. Alternativ kann man auch den ersten Buchstaben einer Zeichenfolge mit der Funktion `string.sub()` ändern.

Eine weitere Möglichkeit besteht darin, benutzerdefinierte Funktionen zu schreiben, die die Groß- und Kleinschreibung von Zeichenfolgen manuell ändern. Dies könnte beispielsweise nützlich sein, wenn man nur den ersten Buchstaben der Zeichenfolge großschreiben möchte, anstatt alle.

In Lua werden alle Zeichen als ASCII-Wert gespeichert, was bedeutet, dass die Groß- und Kleinschreibung von Buchstaben durch ihre numerischen Äquivalente definiert wird. Beim Kapitalisieren einer Zeichenfolge wird also jeder ASCII-Wert um einen bestimmten Wert erhöht, um vom Kleinschreiben zum Großschreiben zu wechseln.

## Siehe auch

Weitere Informationen zu Strings in Lua und deren Manipulation finden Sie in der offiziellen Dokumentation: https://www.lua.org/manual/5.3/manual.html#6.4