---
title:                "Att hitta längden av en sträng"
html_title:           "Fish Shell: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng är en vanlig uppgift för programmerare. Det innebär helt enkelt att räkna antalet tecken i en sträng, inklusive mellanslag och specialtecken. Detta är användbart för att manipulera och hantera strängar på ett effektivt sätt i våra program.

## Så här:
```Fish Shell
set sträng = "Hej världen!"
echo "(count $sträng)"
```
Output:
```
13
```

## Djupdykning:
Att räkna längden på en sträng har varit en del av programmering sedan de tidiga dagarna av datorer. Det finns många olika sätt att göra det, men i Fish Shell använder vi kommandot ```count``` för att göra det på enklaste sätt. Alternativet är att använda inbyggda funktioner i ditt programmeringsspråk, som till exempel ```len()``` i Python eller ```length()``` i JavaScript.

## Se även:
- [Fish Shell dokumentation för count command](https://fishshell.com/docs/current/cmds/count.html)
- [Skillshare kurs: Introduction to Fish Shell](https://www.skillshare.com/classes/Introduction-to-Fish-Shell-An-Interactive-Command-Line-Utility/1678268081)
- [Youtube tutorial: String Length in Programming Languages](https://www.youtube.com/watch?v=UoSqUJpXfJY)