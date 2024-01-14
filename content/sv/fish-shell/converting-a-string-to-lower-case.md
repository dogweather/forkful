---
title:    "Fish Shell: Omvandla en sträng till gemener"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför
Att konvertera en sträng till gemener kan vara användbart för att förbättra användarvänligheten i ditt Fish Shell-program. Genom att göra alla bokstäver i en sträng till gemener kan du säkerställa att användare inte skriver in felaktig case-sensitiv input.

## Så här
För att konvertera en sträng till gemener i Fish Shell, använder du kommandot `string tolower`. Här är ett exempel på hur man koder det:

```
Fish Shell
echo "Hej, VÄRLDEN!" | string tolower
```

Outputen blir `hej, världen!` eftersom alla bokstäver har konverterats till gemener. Detta kommando fungerar också med variabler och andra strängar som input.

## Deep Dive
För att förstå mer om hur `string tolower`-kommandot fungerar, kan du titta på dess dokumentation genom att skriva `man string` i Fish Shell. Detta kommando tar dig till manualsidan för `string`-kommandot, där du kan läsa mer om alla dess funktioner och möjligheter.

En annan intressant funktion är användningen av flaggan `-r` med `string tolower`. Den gör att kommandot konverterar alla bokstäver till gemener i en rekursiv sträng (en sträng som innehåller andra strängar). Detta kan vara användbart vid komplexare program där du behöver hantera stora mängder av data.

## Se även
- [Dokumentation för `string`-kommandot (på engelska)](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shells officiella hemsida (på engelska)](https://fishshell.com/)
- [En guide till Fish Shell för nybörjare (på engelska)](https://dev.to/bbavouzet/a-brief-introduction-to-fish-shell-43p6)