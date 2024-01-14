---
title:                "Gleam: Att arbeta med CSV-formatet"
simple_title:         "Att arbeta med CSV-formatet"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## Varför

CSV är en vanlig filtyp som används för att lagra data i tabellform. Att kunna arbeta med CSV i dina Gleam-program öppnar upp möjligheten att manipulera, filtrera och analysera stora mängder data. Detta kan vara till stor hjälp för forskare, dataanalytiker, eller bara för att organisera och hantera data i ditt program.

## Hur man gör

För att arbeta med CSV i Gleam behöver du först importera `gleam/csv` biblioteket. Sedan kan du läsa in en CSV-fil med funktionen `read_file` och spara den i en variabel.

```
import gleam/csv

let csv = csv.read_file("data.csv")
```

För att få ut datan från CSV-filen kan du använda funktionen `parse` tillsammans med den sparade variabeln. Den returnerar en lista med listor som representerar varje rad i CSV-filen.

```
let data = csv.parse()
```

Om ditt CSV-dokument har headers kan du hämta dessa med funktionen `headers`.

```
let headers = csv.headers()
```

Nu kan du använda Gleams inbyggda funktioner som `list.map` och `list.filter` för att manipulera och filtrera datan på olika sätt.

```
let filtered_data = list.filter(data, _ -> .id == "123")

let mapped_data = list.map(data, row -> {
  .name = row.name,
  .age = row.age,
  .city = row.city
})
```

## Djupdykning

För att ge dig mer kontroll över hur du läser in och hanterar data från CSV-filer finns det många konfigurationsalternativ som du kan använda i `csv.read_file`. Till exempel kan du ange separatorn mellan kolumner, ignorera rader utan data och välja vilka kolumner som ska inkluderas.

Du kan också använda funktionerna `to_map` och `to_row` för att konvertera datan till en mer användarvänlig form. `to_map` skapar en map med kolumnnamn som nycklar och värden från varje rad som värden. `to_row` gör samma sak, men returnerar en rad i stället för en map.

## Se även

- [Gleams officiella dokumentation för CSV](https://gleam.run/documentation/std/csv.html)
- [En guide för hur man arbetar med CSV i Gleam](https://gleam.run/news/arbeta-med-csv-filer-i-gleam.html)
- [En tutorial om hur man importerar och arbetar med data i Gleam](https://meltwater.github.io/guide/guide-data-import.html)