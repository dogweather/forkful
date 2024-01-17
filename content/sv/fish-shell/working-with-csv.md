---
title:                "Arbeta med csv"
html_title:           "Fish Shell: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

# Vad & Varför?
CSV står för Comma Separated Values och är ett vanligt filformat som används för att lagra tabelldata. Det används ofta av programmerare för att hantera och manipulera stora mängder data på ett enkelt sätt. Det är också ett vanligt format för att exportera och importera data mellan olika program.

# Hur?
Fish Shell innehåller inbyggda funktioner som gör det enkelt att arbeta med CSV-filer. Här är några exempel på hur du kan använda Fish Shell för att hantera CSV-data:

### Skriv ut en CSV-fil
```Fish Shell
cat file.csv
```
Detta kommer att skriva ut innehållet i filen "file.csv" i konsolen.

### Sortera en CSV-fil
```Fish Shell
sort -k 2 file.csv
```
Detta kommer att sortera filen "file.csv" baserat på andra kolumnen och skriva ut den nya ordningen i konsolen.

### Välj specifika kolumner
```Fish Shell
cut -d "," -f 1,3 file.csv
```
I detta exempel väljer vi bara första och tredje kolumnen i filen "file.csv" och skriver ut dem i konsolen.

# Djupdykning
CSV-formatet har funnits sedan 1972 och är ett av de vedertagna formaten för tabelldata. Tidigare användes det främst för att överföra data mellan olika databaser, men det har nu blivit allt vanligare även för andra typer av data.

Det finns flera andra filformat som också används för att lagra tabelldata, som t.ex. JSON och XML. Dessa format har vissa fördelar jämfört med CSV, som t.ex. att de kan hantera mer komplex data. Men för enkla tabeller är CSV fortfarande ett mycket användbart och effektivt format.

Fish Shell använder ett externt kommando kallat "csvkit" för att hantera CSV-filer. Detta kommando tillhandahåller olika verktyg för att hantera och manipulera CSV-data, som t.ex. "csvsort", "csvcut" och "csvgrep". Du kan läsa mer om dessa kommandon och deras användning genom att köra "man csvkit" i terminalen.

# Se även
- [Fish Shell dokumentation](https://fishshell.com/docs/current/index.html)
- [Csvkit dokumentation](https://csvkit.readthedocs.io/en/latest/)
- [Wikipedia - CSV](https://en.wikipedia.org/wiki/Comma-separated_values)