---
title:                "Att få dagens datum"
html_title:           "Gleam: Att få dagens datum"
simple_title:         "Att få dagens datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna få den nuvarande datumet är en viktig komponent av programmering. Det tillåter dig att skapa dynamiska och tidsbaserade applikationer och är en viktig del av att hantera tidrelaterade data.

## Hur man
Att få den nuvarande datumet i Gleam är enkelt. Först kan du importera 'Time' -modulen med `import Time`. Sedan kan du använda funktionen 'now' för att få den nuvarande tidpunkten som en sträng.

```Gleam
import Time

let currentDate = Time.now()
```

Om du vill anpassa hur datumet visas kan du använda funktionen `format`, som tar emot en sträng som ett argument och returnerar datumet i det önskade formatet. Till exempel:

```Gleam
let formattedDate = Time.format("%A, %B %d, %Y")
```

Detta kommer att returnera datumet i formatet "dag, månad dag, år", till exempel "måndag, maj 14, 2021".

## Deep Dive
För de som är intresserade av att veta mer om hur datumet hanteras i Gleam finns det flera inbyggda funktioner som kan vara användbara. Som nämnts tidigare kan du använda `now` för att få den nuvarande tiden, men du kan också använda `utc_now` för att få den aktuella tiden i UTC-format. Du kan också använda `add_seconds` och `add_minutes` för att lägga till tidsenheter till din nuvarande tid. Slutligen, om du vill hantera specifika datum, kan du använda `from_utc` som tar emot år, månad, dag och tid som argument och returnerar en tidsstämpel.

## Se även
- Officiell Gleam-hemsida: https://gleam.run/
- Gleam dokumentation: https://gleam.run/get-started/introduction.html