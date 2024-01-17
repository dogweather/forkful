---
title:                "Omvandling av en sträng till gemener"
html_title:           "Fish Shell: Omvandling av en sträng till gemener"
simple_title:         "Omvandling av en sträng till gemener"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera en sträng till gemener (lower case) innebär att alla bokstäver i strängen omvandlas till små bokstäver. Detta kan vara användbart för att jämföra strängar och göra programmet mer lättläst.

## Hur?
Att konvertera en sträng till gemener i Fish Shell är enkelt. Använd kommandot `string tolower` och ange strängen du vill konvertera som argument. Här är ett exempel:

```Fish Shell
set sträng "HEJ HEJ"
echo (string tolower $sträng)
```

Output: `hej hej`

## Djupdykning
Att konvertera en sträng till gemener är en vanlig operation inom programmering. Förr i tiden användes olika metoder beroende på vilket språk som användes, men idag finns det inbyggda funktioner i många programmeringsspråk som gör detta enkelt. I Fish Shell används kommandot `string tolower`, men i andra språk kan det heta något annat, som `tolower()` i C++.

Det finns också alternativa sätt att göra samma sak, som att använda `tr`-kommandot eller regelbundna uttryck. Men det beror på personliga preferenser och vilket språk som används.

En viktig aspekt att tänka på när man konverterar en sträng till gemener är skillnaden mellan stora och små bokstäver i olika språk. Till exempel är det inte alltid enkelt att konvertera en sträng till gemener på ett korrekt sätt i svenska då vissa bokstäver som åäö kan ha olika representationer i gemener och versaler.

## Se även
- Fish Shell dokumentation för `string` kommandot: https://fishshell.com/docs/current/cmds/string.html
- Utvecklarens guide för programmering i Fish Shell: https://fishshell.com/docs/current/index.html