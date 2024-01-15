---
title:                "Omvandla ett datum till en sträng"
html_title:           "Python: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är ett vanligt problem som Python-utvecklare kommer att stöta på under sin karriär. Det finns olika situationer där denna konvertering kan behöva göras, till exempel för att spara datumet i en fil eller för att visa det i ett brukarvänligt format för användaren. I detta artikel kommer vi att gå igenom hur man konverterar ett datum till en sträng i Python och ge en djupare förståelse av hur denna process fungerar.

## Så här gör du

Den grundläggande metoden för att konvertera ett datum till en sträng är att använda `strftime()`-funktionen i `datetime`-modulen. Syntaxen är enkel:

```
Python från datetime import datetime
nu = datetime.now()
datum_sträng = nu.strftime('%d/%m/%Y %H:%M:%S')
```

I detta exempel använder vi `strftime()` för att konvertera det aktuella datumet och tiden till en sträng, som sedan sparas i variabeln `datum_sträng`. Som du kan se från formatet `'%d/%m/%Y %H:%M:%S'`, kan du anpassa strängens utseende genom att ange olika variabler och deras placering. Tabellen nedan visar de vanligaste variablerna som kan användas för att formatera en datumsträng:

| Variabel | Beskrivning                      | Exempel |
|----------|----------------------------------|---------|
| `%d`     | Dag i månaden (01-31)            | 01      |
| `%m`     | Månad (01-12)                    | 09      |
| `%Y`     | År (heltal)                      | 2021    |
| `%H`     | Timme (00-23)                    | 14      |
| `%M`     | Minut (00-59)                    | 30      |
| `%S`     | Sekund (00-59)                   | 45      |
| `%A`     | Veckodag (hela namnet)           | Måndag  |
| `%b`     | Månad (förkortning)              | Sep     |
| `%p`     | AM/PM                            | AM      |

Det finns också andra formatalternativ som kan användas för att visa datum och tid, som är mer specifika för vissa länder eller språk. Du kan hitta en komplett lista över dessa formatalternativ i Python-dokumentationen för `strftime()`.

## Djupdykning

För att förstå hur denna konverteringsprocess fungerar, är det viktigt att förstå hur Python representerar datum och tid i `datetime`-modulen. När vi använder `datetime.now()` får vi en `datetime`-objekt som innehåller information om det aktuella datumet och tiden. Detta objekt har sedan olika metoder för att konvertera datumen och tiderna till olika format. En av dessa metoder är `strftime()`, som vi använder för att konvertera datumet till en sträng.

`strftime()` funktionen har två argument: det första är det datumobjekt som ska omvandlas och det andra är det önskade formatet för strängen. När funktionen körs, använder den datumobjektets interna värden och matchar dem mot det specificerade formatet för att skapa strängen.

Det är viktigt att komma ihåg att `strftime()` inte bara används för att konvertera datum utan kan också användas för att formatera tiden, datumet och tiden, eller endast tiden. Du kan experimentera med olika kombinationer av formatalternativ för att få önskat utseende för din sträng.

## Se även

- [Python dokumentation för datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [Tutorial för att konvertera ett datum till en sträng i Python](https://realpython.com/python-datetime/)