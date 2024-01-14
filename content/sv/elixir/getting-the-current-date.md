---
title:                "Elixir: Att få aktuellt datum"
simple_title:         "Att få aktuellt datum"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda det aktuella datumet är en viktig del av programmering i Elixir. Det kan vara användbart för att skapa dynamiska funktioner, hantera tidsstämplar i databaser eller för att helt enkelt visa rätt datum på en användargränssnitt.

## Hur man gör det

För att hämta det aktuella datumet i Elixir, kan du använda funktionen `Date.utc_today/0`. Detta kommer att returnera ett `Date`-objekt som representerar dagens datum i UTC-tid.

```Elixir
Date.utc_today()
```

Det är också möjligt att göra anpassade förfrågningar, som att hämta datumet för en specifik tidszon eller konvertera datumet till en annan tidszon. Detta kan göras genom att använda funktionen `Date.from_erl/2` tillsammans med Elixirs standardbibliotek `Calendar`.

```Elixir
# Hämta dagens datum i tidszonen för Stockholm
{year, month, day} = Date.utc_today()
{:ok, stockholm_date} = Calendar.from_erl({year, month, day}, "Europe/Stockholm")
```

Det är också möjligt att formatera det aktuella datumet baserat på dina preferenser. Detta kan göras med hjälp av `Calendar`-biblioteket och funktionen `Calendar.strftime/2`.

```Elixir
# Formatera dagens datum som "27 juli 2021"
{year, month, day} = Date.utc_today()
"#{Calendar.strftime({year, month, day}, "%e %B %Y")}"
```

## Djupdykning

Det är viktigt att notera att funktionen `Date.utc_today/0` hämtar datumet i UTC-tid, som är standardtiden för Elixir. Om du vill ha det aktuella datumet i en annan tidszon, bör du använda funktionerna `Date.utc_today!/0` eller `Date.from_erl!/2` för att undvika oönskade tidsändringar.

Det är också viktigt att hålla i minnet att `Date`-objekt är oföränderliga och alltid återgår till UTC-tid vid användning av operators som `+` och `-`. För att ändra datumet, bör du använda funktioner som `Date.add/2` eller `Date.subtract/2`.

## Se även

- Elixir Kalenderbibliotekets dokumentation: [https://hexdocs.pm/elixir/Calendar.html](https://hexdocs.pm/elixir/Calendar.html)
- Officiell Elixir-dokumentation för Datummodulen: [https://hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
- Elixir-skola om datumbehandling: [https://exercism.io/tracks/elixir/concepts/dates](https://exercism.io/tracks/elixir/concepts/dates)