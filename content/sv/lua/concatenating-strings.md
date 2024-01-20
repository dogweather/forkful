---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Sammanslagning av strängar i programmering handlar om att länka ihop två eller flera textsträngar till en enda. Programmerare gör detta för att manipulating data, format output och generera dynamiska strengar.

## Såhär gör man:
I Lua, använd `..` för att slå ihop strängar. Försök med följande exempel:

```Lua
str1 = "Hej, "
str2 = "Sverige!"
sammanslagning = str1 .. str2
print(sammanslagning)
```

Output:

```Lua
Hej, Sverige!
```

## Djupdykning
Strängsammanfogning i Lua har sina rötter i lägre nivå programmeringsspråk som C. Tidigare, många språk använde en teknik som kallas "buffer concatenation", men Lua använder `..` operatorn som ett mer intuitivt och effektivt alternativ.

Alternativ till `..` inkluderar `string.format()` och `table.concat()`. `string.format()` är användbart när du vill infoga värden i en sträng, medan `table.concat()` är effektiv för att sammanfoga stora datamängder.

Strängsammanfogning i Lua är implementerad genom att skapa en ny sträng och kopiera de gamla strängarna till den. Observera att detta kan vara kostsamt vid stora datamängder.

## Se Även
- [Lua String Library](http://www.lua.org/manual/5.1/manual.html#5.4)
- [Lua Users Wiki: Strings Tutorial](http://lua-users.org/wiki/StringsTutorial) 
- [Lua String concatenation performance](https://stackoverflow.com/questions/987772/lua-string-concatenation-performance)