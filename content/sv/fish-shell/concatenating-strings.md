---
title:                "Sammanslagning av strängar"
html_title:           "Fish Shell: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konkatenera strängar innebär att man lägger samman flera strängar för att skapa en ny, längre sträng. Programörer gör detta för att skapa dynamiska och anpassningsbara texter, till exempel för användarinteraktion eller för att manipulera data.

## Så här:

Fish Shell erbjuder flera enkla sätt att konkatenera strängar. Se nedan för exempelkod och utdata.

```
set greeting "Hej" " " "världen!"
echo $greeting
# Output: Hej världen!
```

```
set name "Lisa"
echo Hej $name"!"
# Output: Hej Lisa!
```

```
set count 5
echo Vi har $count "fiskar i akvariet."
# Output: Vi har 5 fiskar i akvariet.
```

## Djupdykning:

Historiskt sett har konkatenering av strängar varit en vanlig operation i programmering, men framsteg inom språk som Ruby och Python har lett till alternativ som interpolering av strängar och formatering av strängar. I Fish Shell finns det också alternativ att använda andra inbyggda funktioner, såsom string join.

Det finns också vissa effektivitetsaspekter att tänka på när man konkatenerar strängar. Till exempel kan det vara bättre att använda en buffer eller en StringBuilder om man ska konkatenera en stor mängd strängar.

## Se även:

[Officiell Fish Shell dokumentation om konkatenering](https://fishshell.com/docs/current/scripting.html#strings)

[En jämförelse av tre olika sätt att hantera strängar i Fish Shell](https://dev.to/echakrab/different-ways-to-handle-string-manipulation-in-fish-shell-3k98)