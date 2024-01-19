---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# "Bli med og kode" – Slik konkatenerer du strenger i Elixir

## Hva & Hvorfor?
Konkatenere strenger handlar om å slå sammen to eller flere strenger til én. Dette er nyttig når man skal bygge og formatere dynamiske meldinger, filbaner og andre typer tekstsammensetninger i koden din.

## Hvordan gjøre det:
Bruk den innebygde funksjonen i Elixir, `<>`, for å konkatenere strenger.
```Elixir
navn = "Olav"
hilsen = "Hei, " <> navn
IO.puts(hilsen)
```
Dette vil skrive ut:
```
Hei, Olav
```

## Dyp-dykk
Elixir er tydelig påvirket av Erlang og andre funksjonelle programmeringsspråk. I disse språkene er optimalisering av strengsammenslåing viktig for å unngå unødvendig minnebruk.

En alternativ og litt mer komplisert metode er å bruke listefunksjoner for å slå sammen lister med tegn, som kan konverteres til en enkelt streng.

```Elixir
navn = ['O','l','a','v']
hilsen = ['H','e','i',',', ' '|navn]
IO.puts(to_string(hilsen))
```

Implementeringen av `<>` bruker denne metoden under lokk og lokk, som er en del av hvorfor Elixir gjør det så lett å arbeide med strenger.

## Se også
- Lær mer om listebehandling i Elixir [her](https://elixir-lang.org/getting-started/io-and-the-file-system.html)
- Les mer om `<>` operatoren [her](https://hexdocs.pm/elixir/String.html) og om sin bruk [her](https://elixir-lang.org/crash-course.html#strings-and-binaries)
- For en grundigere forståelse av Elixir sine strengoperasjoner, sjekk ut denne kilden [her](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)