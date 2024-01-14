---
title:    "Gleam: Konvertera ett datum till en sträng"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera datum till strängar är en vanlig uppgift inom programmering och kan vara användbart för att visa datum på ett läsbart och tydligt sätt. Genom att börja använda Gleam-språket kan du enkelt lära dig att utföra denna uppgift och få en bättre förståelse för hur språket fungerar.

## Så här gör du

För att konvertera ett datum till en sträng i Gleam, behöver du först skapa ett `DateTime` objekt med det önskade datumet. Sedan kan du använda funktionen `DateTime.to_string()` för att konvertera det till en sträng.

```Gleam
let date = DateTime.make(2021, 10, 31)
let date_string = DateTime.to_string(date)
```

Detta kommer att resultera i en sträng med formatet "2021-10-31", vilket är det standardformat som används för datum i Gleam. Du kan också ange ett annat format genom att använda `DateTime.format()` funktionen och ange det önskade formatet som en parameter. Exempelvis kan detta vara användbart om du vill ha ett mer läsbart datum, som "31 oktober 2021".

```Gleam
let date = DateTime.make(2021, 10, 31)
let date_string = DateTime.format(date, "%d %B %Y")
```

Det finns många olika formatmallar som du kan använda för att få den önskade formateringen för ditt datum.

## Djupdykning

Det finns mer att lära sig om att konvertera datum till strängar i Gleam. Till exempel kan du också hantera tidszon och lokalisering för dina datum. Du kan även använda funktionen `DateTime.parse()` för att konvertera en sträng till ett `DateTime` objekt. Genom att utforska dessa olika funktioner kan du anpassa din kod och göra mer avancerade funktioner som behövs för dina projekt.

Se även

- Gleam:s dokumentation om datum och tider: https://gleam.run/documentation/stdlib/datetime
- En guide till hur man använder Gleam språket: https://medium.com/@justlikethebutter/gleam-programming-language-tutorial-64a5f4a0fa8e
- En samling av vanliga kodsnuttar i Gleam: https://github.com/gleam-lang/gleam-snippets