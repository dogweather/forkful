---
title:                "Omvandla en datum till en sträng"
html_title:           "Elixir: Omvandla en datum till en sträng"
simple_title:         "Omvandla en datum till en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera en datum till en sträng är en viktig del av programmering eftersom det hjälper till att presentera datum och tid på ett användarvänligt sätt. Det är också en användbar funktion när man arbetar med datahantering och lagring.

## Hur man gör det
```Elixir
defp convert_date_to_string(date) do
  date |> NaiveDateTime.from_iso8601! |> NaiveDateTime.to_string("YYYY-MM-DD")
end
```

Genom att använda funktionerna `NaiveDateTime.from_iso8601!` och `NaiveDateTime.to_string` kan vi konvertera ett datum till en sträng i det önskade formatet. I exemplet ovan använder vi formatet "YYYY-MM-DD", men det finns många andra format att välja mellan.

Exempel på input och output:

```Elixir
iex> convert_date_to_string("2020-07-15")
"2020-07-15"
```

## Utforska djupare
Att konvertera en datumsträng till ett specifikt format kan också användas för att jämföra och sortera datum i en databas. Det finns också olika bibliotek tillgängliga i Elixir för att hantera datum och tider, till exempel `timex` och `calendar`.

## Se även
- [Elixir Date and Time](https://elixir-lang.org/getting-started/basic-types.html#dates-and-times)
- [timex documentation](https://hexdocs.pm/timex/Timex.html)
- [calendar documentation](https://elixir-lang.org/docs/1.11/calendar.html)