---
title:    "Elixir: Søke og erstatte tekst"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å søke og erstatte tekst er en viktig del av programmering. Det lar deg raskt og enkelt gjøre endringer i store mengder tekst, noe som sparer deg for tid og gjør koden din mer effektiv.

## Hvordan å gjøre det

Søke og erstatte funksjonene i Elixir er enkle å bruke, men de kan være veldig kraftige. La oss se på et eksempel der vi ønsker å endre alle forekomster av "hello" til "hei" i en streng:

```elixir
s = "hello world"
new_s = String.replace(s, "hello", "hei")
IO.puts new_s
```

Output vil være:

```elixir
hei world
```

Som du kan se, ble "hello" erstattet med "hei" i teksten. Du kan også søke og erstatte på flere forekomster ved å bruke en liste av verdier, for eksempel:

```elixir
s = "hello world, hello there"
new_s = String.replace(s, ["hello", "there"], ["hei", "der"])
IO.puts new_s
```

Output vil være:

```elixir
hei world, hei der
```

Du kan også bruke regulære uttrykk når du søker og erstatter tekst. La oss si at vi ønsker å endre alle tall i en streng til ordet "nummer". Vi kan bruke regulære uttrykket `\d+` som betyr å matche en eller flere sifre. Se på følgende eksempel:

```elixir
s = "I have 5 apples and 3 oranges"
new_s = Regex.replace(~r/\d+/, s, "nummer")
IO.puts new_s
```

Output vil være:

```elixir
I have nummer apples and nummer oranges
```

Se dokumentasjonen for mer informasjon om alle mulighetene du har med søke og erstatte funksjoner i Elixir.

## Dypdykk

Det er viktig å merke seg at når du søker og erstatter tekst, blir den originale variabelen ikke endret, men en ny kopi blir returnert. Dette er fordi strenger er uforanderlige i Elixir, noe som betyr at de ikke kan endres direkte. Derfor er det viktig å tilordne den nye variabelen til en eksisterende variabel eller bruke den i en funksjon.

## Se også
- [Elixir String-modulen dokumentasjon] (https://hexdocs.pm/elixir/String.html)
- [Elixir Regex-modulen dokumentasjon] (https://hexdocs.pm/elixir/Regex.html)
- [Elixir String og Regex Cheat Sheet] (https://devhints.io/elixir-strings-regex)