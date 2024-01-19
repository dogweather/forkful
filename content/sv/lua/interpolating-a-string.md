---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Stränginterpolation i Lua

## Vad & Varför?
Stränginterpolation är en teknik för att infoga variabler direkt i en sträng. Med detta kan programmerare skapa dynamiska meddelanden utan att ständigt behöva konkatenera sträng och variabler.

## Hur man:
I Lua, samsas stränginterpolation vanligtvis med string.format funktionen. Här är ett enkelt exempel:

```Lua
name = 'Kalle'
age = 23
print(string.format("Hej, jag heter %s och jag är %d år gammal.", name, age))
```

Output:

```Lua
Hej, jag heter Kalle och jag är 23 år gammal.
```

## Djupgående 
Historiskt sett lanserades stränginterpolation i Lua 5.1 och användningen har ökat sedan dess. Det finns alternativ till `string.format`, till exempel `..`-operatören för strängkonkatenering, men de är oftast mer verbose och svårare att läsa.

Lua implementerar stränginterpolation genom att använda interna C-biblioteksfunktioner, vilket gör det snabbt och effektivt.

## Se även
För mer information, referera till följande länkar:
- Stränginterpolation i Lua Dokumentation: [Lua 5.1 String Interpolation](https://www.lua.org/manual/5.1/manual.html#5.4)
- Lua String Bibliotek Dokumentation: [Lua 5.1 String Library](https://www.lua.org/manual/5.1/manual.html#5.4) 
- Djup dykning i Lua strängar: [Hacking With Lua Strings](http://lua-users.org/wiki/StringsTutorial)
- Lua källkod på GitHub: [Lua Source Code](https://github.com/lua/lua)