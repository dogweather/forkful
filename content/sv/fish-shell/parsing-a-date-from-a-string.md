---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Hantera datum från strängar med Fish Shell

## Vad och Varför?

Att tolka ett datum från en sträng innebär att omvandla textuell data till ett datumsobjekt. Programmerare gör det för att enkelt hantera och manipulera datumdata i sina program.

## Hur till:

Fish Shell är ett kraftfullt verktyg för att hjälpa till med detta. Här är några kodblok med exempel:

```fish
# Skapa en sträng
set -l mydate "2022-03-21"

# Omvandla strängen till ett datum
set -l parsed_date (date -u -d $mydate "+%Y-%m-%d")

# Skriv ut det tolkade datumet
echo $parsed_date
```
Resultat:
```
2022-03-21
```

## Djupdykning

Historiska sammanhang: Datumtolkning är ingen ny ide; redan i tidiga programmeringsspråk behövde man ordna datum från textdata. Fish Shell har gjort processen mycket enklare och mer intuitiv.

Alternativ: Det finns andra skal som Bash och Zsh som kan göra samma saker, men Fish Shell skiljer sig ut för dess användarvänlighet.

Verkställighetsdetaljer: När du skriver `date -u -d $mydate "+%Y-%m-%d"`, ber du `date`-kommandot att tolka värdet i `mydate`-variabeln som ett datum och sedan returnera det formatet som en sträng i `YYYY-MM-DD`-formatet.

## Se också:

Du kan läsa mer om datum i Fish Shell i dess dokumentation: [Fish Docs - Date Command](https://fishshell.com/docs/current/commands.html#date)

För att förstå mer om strängmanipulation, ta en titt på denna artikel: [Fish Shell - String Operations](https://fishshell.com/docs/current/commands.html#string)

För mer detaljerad förståelse kring datumformatering kan du besöka: [Fish - Date formatting](https://fishshell.com/docs/current/tutorial.html#date)