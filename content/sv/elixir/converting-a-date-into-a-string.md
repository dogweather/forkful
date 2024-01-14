---
title:                "Elixir: Omvandla ett datum till en sträng"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
I denna blogginlägg kommer vi att gå igenom hur man konverterar datum till strängar i Elixir. Detta är en viktig del av programmering eftersom datum ofta används för att representera tid och det är viktigt att kunna arbeta med dem på ett effektivt sätt.

## Hur man gör
För att konvertera datum till strängar i Elixir kan vi använda funktionen `to_string` tillsammans med en formateringssträng. Här är ett exempel:

```elixir
# Skapar ett datum-objekt för 1 januari 2021
date = ~D[2021-01-01]

# Konverterar datumet till en sträng med formatet "YYYY-mm-dd"
date_str = to_string(date, "YYYY-mm-dd")

IO.puts(date_str) # Output: "2021-01-01"
```

Som du kan se i exemplet ovan måste vi ange ett format som passar vårt behov. Detta kan innehålla olika kombinationer av år, månad och dag, beroende på vad vi vill ha med i vår slutgiltiga sträng. Här är några av de vanligaste formateringssträngarna:

- `YYYY` - Fyra siffror för året
- `YY` - Två siffror för året
- `mm` - Två siffror för månaden
- `dd` - Två siffror för dagen

Det finns också flera andra mönster som vi kan använda, beroende på vilken information vi vill ha med eller exakt hur vi vill formatera vår sträng. Mer information om detta kan hittas i Elixirs officiella dokumentation för `Calendar`.

## Djupdykning
När vi konverterar datum till strängar är det viktigt att förstå vilken tidszon som är inställd på vårt system. Om vi inte specifikt anger en tidszon i vår konvertering, kommer den att använda den systeminställda tidszonen. Det kan resultera i att strängen ser annorlunda ut än vad vi förväntade oss om vi arbetar i ett annat tidszon.

Det är också värt att notera att om vi försöker konvertera ett ogiltigt datum, t.ex. 30 februari, kommer det att generera ett undantag. Därför är det bra att alltid kontrollera att våra datum är giltiga innan vi konverterar dem till strängar.

## Se även
- Elixirs officiella dokumentation för `Calendar`: https://hexdocs.pm/elixir/Calendar.html
- Mer om formateringssträngar för datum: https://hexdocs.pm/elixir/Calendar.html#strftime-and-strftime-1
- Diskussion om tidszoner i Elixir: https://elixirforum.com/t/time-zones-in-elixir-ecto/26566