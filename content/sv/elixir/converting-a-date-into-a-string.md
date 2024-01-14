---
title:    "Elixir: Konvertera ett datum till en sträng"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Elixir är ett kraftfullt programmeringsspråk som lämpar sig väl för webbapplikationer, backend-system och distribuerade system. Ett vanligt problem som många utvecklare ställs inför är att konvertera datum till strängar för att visa dem på användargränssnittet eller spara dem i en databas. I denna bloggpost kommer vi att titta på hur man kan konvertera datum till strängar i Elixir, vilket är ett viktigt verktyg för att arbeta med datum och tidsdata.

## Så här gör du

Vi börjar med att importera Elixir-modulen `Calendar` som innehåller funktioner för att hantera datum och tider.

```Elixir
import Calendar
```

För att konvertera ett datum till en sträng kan vi använda funktionen `to_string` tillsammans med en specifik formatsträng. Till exempel, om vi vill visa datumet i formatet "åååå-mm-dd", kan vi skriva:

```Elixir
date = ~D[2021-10-25]
to_string(date, "~4..-~2..-~2..")
```

Det första argumentet är det datum som vi vill konvertera, och det andra argumentet är formatet som vi vill att det ska visas i. I detta fall använder vi `~4` för att ange att året ska visas med 4 siffror, `~2` för att visa månad och dag med 2 siffror och `..` för att separera delarna.

Om vi vill lägga till tidsinformation i vår sträng, kan vi använda `to_string/3`:

```Elixir
time = ~T[13:30:00]
to_string(date, "~4..-~2..-~2.. ~2.~2.~2.", time)
```

Här använder vi `~2.` för att visa timme, minut och sekund med 2 siffror och separera dem med punkter.

Det finns många olika formatsträngar som kan användas för att konvertera datum till strängar. En fullständig lista finns i Elixirs dokumentation.

## Djupdykning

När vi konverterar datum till strängar med Elixir används faktiskt modulen `DateTime` bakom kulisserna. Detta är en datatyp som kombinerar datum och tid i en enda struktur. `DateTime` har även funktioner för att konvertera ett datum till en annan tidszon och hantera tidszoner generellt.

En annan användbar funktion i `DateTime`-modulen är `to_iso8601`, som konverterar datumet till det internationella ISO 8601-formatet. Detta är ett standardformat som används i många länder och är användbart om du behöver kommunicera med andra system som stöder detta format.

## Se även

- Elixir dokumentation för `Calendar`: https://hexdocs.pm/elixir/Calendar.html
- Elixir dokumentation för `DateTime`: https://hexdocs.pm/elixir/DateTime.html