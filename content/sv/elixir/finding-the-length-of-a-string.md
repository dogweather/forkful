---
title:                "Elixir: Att hitta längden på en sträng"
simple_title:         "Att hitta längden på en sträng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande del av programmering och kan vara användbart för att hantera och manipulera textdata. I denna bloggpost kommer vi att titta på hur man enkelt kan göra detta med hjälp av Elixir.

## Så här gör du

För att hitta längden på en sträng i Elixir, kan du använda funktionen `String.length()`. Den tar en sträng som inmatning och returnerar antalet tecken i strängen.

```Elixir
iex> String.length("Hej!")
3
iex> String.length("32b karaktärer")
14
```

Som du kan se från exemplet ovan, tar funktionen hänsyn till alla tecken i strängen, inklusive mellanslag och specialtecken.

För att inkludera mellanslag och specialtecken som en faktor i längden, kan du använda `byte_size()` funktionen istället.

```Elixir
iex> byte_size("Hej!")
4
iex> byte_size("32b karaktärer")
16
```

Den här funktionen returnerar antalet byte som används för strängens koder. För de flesta skriftsystem, kommer byte_size() att returnera samma värde som String.length(). Men om du arbetar med unicode eller olika språk, kan det vara viktigt att använda `byte_size()` för att få en exakt längd.

## Djupdykning

Det kan vara intressant att veta hur Elixir hanterar strängar under huven för att förstå varför vi använder `String.length()` och `byte_size()`. Elixir strängar representeras som en lista av heltal som motsvarar koder för varje tecken. Detta innebär att längden på en sträng är lika med antalet element i listan.

```Elixir
iex> "Hej!" |> String.to_charlist
[72, 101, 106, 33]
```

Funktionen `String.to_charlist` konverterar vår sträng till en lista av heltal, där varje heltal representerar ett teckens kod.

För att räkna antalet element i en lista kan du använda `Enum.count()` funktionen.

```Elixir
iex> [72, 101, 106, 33] |> Enum.count()
4
```

Nu vet du hur Elixir räknar längden på en sträng och hur du kan använda funktionerna `String.length()` och `byte_size()` för att få längden baserat på ditt användningsfall.

## Se även

- [Elixir dokumentation för String module](https://hexdocs.pm/elixir/String.html)
- [Elixir dokumentation för Enum module](https://hexdocs.pm/elixir/Enum.html)
- [Elixir skolan: Data typer](https://elixirschool.com/en/lessons/basics/basics/data-types/)