---
title:    "Elixir: Jämföra två datum"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart i många programmeringsprojekt, till exempel för att kontrollera om en specifik händelse inträffade före eller efter ett annat tillfälle.

## Hur man gör

Jämförelse av två datum i Elixir kan göras enkelt med hjälp av jämförelseoperatorn `==` och en konvertering av datumen till Elixir-tidsstrukturer. Här är ett exempel som jämför två datum och ger ett utdatavärde baserat på resultatet:

```Elixir
date_1 = ~D[2020-05-01]
date_2 = ~D[2020-05-05]

if date_1 == date_2 do
  IO.puts "Båda datumen är samma."
else
  IO.puts "Datumen är inte samma."
end
```

I det här fallet kommer det att skrivas ut "Datumen är inte samma." eftersom `date_1` och `date_2` är två olika datum.

## Djupdykning

I Elixir motsvaras datum av en Elixir-tidsstruktur som heter `NaiveDateTime`. Detta gör det möjligt att jämföra datumen med hjälp av olika inbyggda funktioner som `==`, `<`, `>` osv. Om du vill göra mer avancerade jämförelser kan du också använda funktioner som `DateTime.compare/2` för att jämföra två datum i detalj.

## Se även

- [Elixir DateTime-modulen](https://hexdocs.pm/elixir/DateTime.html)
- [Jämförelse av datum i Elixir](https://elixir-lang.org/getting-started/modules.html#comparing-and-sorting)