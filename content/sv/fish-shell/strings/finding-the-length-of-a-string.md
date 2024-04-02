---
date: 2024-01-20 17:47:14.971785-07:00
description: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den. Programmerare g\xF6r det f\xF6r att validera indata, begr\xE4nsa\
  \ text eller f\xF6r bearbetning\u2026"
lastmod: '2024-03-13T22:44:38.326512-06:00'
model: gpt-4-1106-preview
summary: "Att hitta l\xE4ngden p\xE5 en str\xE4ng inneb\xE4r att r\xE4kna antalet\
  \ tecken i den. Programmerare g\xF6r det f\xF6r att validera indata, begr\xE4nsa\
  \ text eller f\xF6r bearbetning\u2026"
title: "Hitta l\xE4ngden p\xE5 en str\xE4ng"
weight: 7
---

## Vad & Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare gör det för att validera indata, begränsa text eller för bearbetning av textdata.

## Hur gör man:
Här är hur du gör det i Fish Shell. Enkel och rak på sak.

```Fish Shell
set string "Hejsan!"
echo "Length: "(string length $string)
```

Sample output:
```
Length: 7
```

## Djupdykning
Förr i tiden var det vanligt att loopa igenom en sträng och räkna tecken för hand i många programmeringsspråk. I Fish Shell görs detta elegant och enkelt med `string` kommandot, som lades till i version 2.3.0. Ett alternativ är att använda `expr` i ett script, men `string` är snabbare och inbyggt. `string length` beräknar tecken snabbt och effektivt, hanterar även Unicode korrekt. Det är viktigt då vissa tecken kan representeras med flera byte.

## Se även
- Fish documentation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- Fish tutorial on string manipulation: [https://fishshell.com/docs/current/tutorial.html#tut_strings](https://fishshell.com/docs/current/tutorial.html#tut_strings)
