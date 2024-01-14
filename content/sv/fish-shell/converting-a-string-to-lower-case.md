---
title:                "Fish Shell: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##Varför

Många programmeringsspråk erbjuder en enkel funktion för att konvertera en sträng till små bokstäver, och Fish Shell är inget undantag. Att kunna göra detta kan underlätta livet för programmerare genom att förenkla jämförelser och sökningar.

##Så här gör du

För att konvertera en sträng till små bokstäver i Fish Shell, används kommandot `string tolower` följt av strängen som ska konverteras. Se ett exempel nedan:

```Fish Shell
string tolower "HEJ, VÄRLDEN!"
```

Output: `hej, världen!`

Det finns också möjlighet att använda flaggan `-v` för att visa resultaten på skärmen istället för att spara dem i en variabel. Se ett annat exempel nedan:

```Fish Shell
set sträng "TESTA DETTA"
string tolower -v $sträng
```

Output: `testa detta`

##Djupdykning

När man använder `string tolower` i Fish Shell händer det egentligen flera saker bakom kulisserna. Först och främst kontrolleras vilken typ av tecken som finns i strängen – om det är stora bokstäver, små bokstäver eller tecken som inte är bokstäver alls. Sedan konverteras alla stora bokstäver till små bokstäver och eventuella tecken som inte är bokstäver tas bort.

Det är också viktigt att notera att konvertering till små bokstäver är språkberoende. Det betyder att resultatet kan variera beroende på vilket språk som används.

##Se även

- [Fish Shell dokumentation om `string tolower`](https://fishshell.com/docs/current/commands.html#string-tolower)
- [En jämförelse av strängfunktioner i olika programmeringsspråk](https://www.computerhope.com/jargon/s/string-functions.htm)

Lycka till med dina Fish Shell-programmeringar! 🐟