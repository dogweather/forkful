---
title:                "Hitta längden på en sträng"
date:                  2024-01-20T17:47:14.971785-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hitta längden på en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

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
