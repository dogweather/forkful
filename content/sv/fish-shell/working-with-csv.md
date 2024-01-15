---
title:                "Att arbeta med csv"
html_title:           "Fish Shell: Att arbeta med csv"
simple_title:         "Att arbeta med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (Comma Separated Values) är en vanligt använd format för att lagra och dela data. Genom att kunna hantera CSV i Fish Shell kan du enkelt hantera och manipulera datafiler utan att behöva använda andra program eller verktyg.

## Hur man gör

För att arbeta med CSV i Fish Shell behöver du först installera ett tillägg, som heter "csv", med hjälp av "fisher" pakethanteraren. Använd följande kommando:

```Fish Shell
fisher install csv
```

När tillägget är installerat kan du börja använda kommandon som är speciellt utformade för att hantera CSV-filer. Här är ett exempel på hur man läser och visar innehållet i en CSV-fil:

```Fish Shell
csv read sample.csv | csv pretty
```

Det här kommandot läser filen "sample.csv" och visar innehållet i ett läsbart format. Du kan också använda "csv clean" kommandot för att ta bort onödiga rader eller kolumner från en CSV-fil.

## Djupdykning

För att utföra avancerade operationer på CSV-filer, kan du använda "csv" tilläggets inbyggda funktioner. Med "csv select" kommandot kan du filtrera data baserat på specifika kriterier. Här är ett exempel på hur man väljer rader från en CSV-fil baserat på ett visst värde i en viss kolumn:

```Fish Shell
csv select --columns "Name, Age" --where "Age = 25" sample.csv
```

"csv select" kommandot kan också användas för att sortera data och välja endast vissa kolumner enligt behov. Det finns också andra kommandon som kan användas för att manipulera och bearbeta CSV-filer i Fish Shell, som "csv merge" och "csv cut".

## Se även

För mer information om Fish Shell och dess tillägg, kolla in följande länkar:

- [Fish Shell Officiell Hemsida](https://fishshell.com/)
- [Fish User Documentation](https://fishshell.com/docs/current/)
- [Fisher Pakethanterare](https://github.com/jorgebucaran/fisher)
- [CSV Tilläggsinformation](https://github.com/ebelitov/csv.fish)