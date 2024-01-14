---
title:    "Gleam: Att få aktuell datum"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet är en grundläggande funktion inom programmering och kan vara användbar i en mängd olika situationer. Oavsett om du behöver spåra tidsstämplar för databashanterade poster eller helt enkelt vill inkludera den aktuella tiden i ett meddelande, så kommer du att behöva veta hur du hämtar den aktuella datumet i ditt program.

## Hur man gör det

Att hämta den aktuella datumet i Gleam är en enkel men viktig process. Här är ett enkelt exempel på hur du skulle göra det:

```Gleam

import gleam/time.{LocalDateTime, TimeZone}

let {year, month, day, hour, minute, second} =
  LocalDateTime.now() |> TimeZone.utc() |> LocalDateTime.to_fields()

```

Det här exemplet använder funktionen `now()` från modulen `gleam/time` för att hämta den aktuella tiden, och sedan omvandlas tiden till en lista med olika fält, som år, månad, dag, timme, minut och sekund, genom att använda funktionen `to_fields()`. Vad du sedan gör med dessa fält är upp till dig!

## Djupdykning

Nu när vi har sett en enkel kod för att hämta den aktuella datumet, låt oss titta på några viktiga saker att tänka på när du arbetar med datum och tider i Gleam.

Först och främst är det viktigt att förstå tidzoner! Beroende på var ditt program körs, kan den aktuella tiden visas annorlunda. Därför använder exemplet ovan funktionen `utc()` för att hämta den aktuella tiden i UTC-tidzonen. Om du behöver tiden i en annan tidzon, kan du använda funktioner som `local()` eller `offset()`.

En annan viktig funktion när det gäller datum och tider är `parse()` som kan användas för att konvertera en strängrepresentation av datumet till en `LocalDateTime`-typ. Detta kan vara särskilt användbart när du behöver läsa och hantera datum från en extern fil eller databas.

## Se även

- Gleam dokumentation om tid: https://gleam.run/documentation/?api=gleam/time
- Officiell Gleam-tutorial: https://github.com/gleam-lang/gleam/blob/master/docs/tutorial.md
- Gleam Community Forum: https://elixirforum.com/c/gleam