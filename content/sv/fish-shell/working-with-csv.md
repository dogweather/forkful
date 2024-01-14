---
title:                "Fish Shell: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV (Comma-Separated Values) är ett vanligt format för att hantera data i en tabellstruktur. Om du arbetar med dataanalyser, webbutveckling eller databasadministration, kan du troligtvis stöta på CSV-filer. Att lära sig att arbeta med CSV i Fish Shell kan hjälpa dig att enkelt hantera och manipulera data i dessa filer.

## Hur man gör

För att kunna arbeta med CSV-filer i Fish Shell behöver du först installera ett tredjepartsprogram som heter `csvkit`. Detta kan göras med hjälp av Fish Shells inbyggda pakethanterare, `fisher`. Kör följande kommando i terminalen för att installera `csvkit`:

```Fish Shell
fisher install csvkit
```

När installationen är klar kan du gå vidare till att använda `csvkit` för att manipulera CSV-filer. Till exempel, om du vill sortera en CSV-fil efter ett visst fält, kan du använda kommandot `csvsort`:

```Fish Shell
csvsort -c field_to_sort filename.csv
```

Du kan också använda `csvgrep` för att filtrera rader baserat på ett visst villkor:

```Fish Shell
csvgrep -c field_to_filter -m "condition" filename.csv
```

I båda dessa kommandon ersätts "field_to_sort" och "field_to_filter" med namnet på det fält du vill sortera eller filtrera efter, och "filename.csv" ersätts med namnet på din CSV-fil.

## Djupdykning

När du arbetar med CSV-filer kan det vara viktigt att förstå skillnaden mellan att ändra själva filen och att bara visa den ändrade versionen i terminalen. För att spara dina ändringar i filen kan du använda kommandot `csvsed`:

```Fish Shell
csvsed -e "modification_commands" filename.csv
```

Detta kommer att tillämpa ändringarna specificerade i "modification_commands" på den angivna filen och spara dem. Om du vill bara visa de ändrade värdena utan att spara dem kan du istället använda kommandot `csvlook`:

```Fish Shell
csvlook filename.csv
```

Detta kommer att formatera din CSV-fil i en snygg tabellstruktur som kan vara lättare att läsa.

## Se även

- [Fish Shell](https://fishshell.com/)
- [csvkit repository](https://github.com/wireservice/csvkit)
- [csvkit dokumentation](https://csvkit.readthedocs.io/en/latest/)
- [Lär dig mer om Fish Shell](https://fishshell.com/docs/)