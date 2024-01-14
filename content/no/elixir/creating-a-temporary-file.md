---
title:                "Elixir: Opprette en midlertidig fil"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer er en vanlig praksis i programmering og kan være nyttig i en rekke ulike situasjoner. Det kan være for å lagre midlertidig data, eller for å skape en buffer mellom ulike deler av koden.

## Hvordan

For å lage en midlertidig fil i Elixir, kan vi bruke funksjonen `Tempfile.open!()` som finnes i biblioteket `File`. Denne funksjonen tar imot en filsti som argument, og oppretter en midlertidig fil på denne plassen.

I eksempelet under opprettes en midlertidig fil kalt "test.txt" i mappen der koden kjører:

```elixir
File.Tempfile.open!("test.txt") |> IO.puts("Dette er en midlertidig fil.")
```

Output:

```
Dette er en midlertidig fil.
```

Midlertidige filer er også nyttige når man ønsker å laste ned en fil på nettet og behandle den på en eller annen måte. I dette tilfellet kan man bruke `HTTPoison` biblioteket for å laste ned filen til en midlertidig lokasjon før man behandler den videre.

## Dypdykk

Det er viktig å merke seg at midlertidige filer kun eksisterer så lenge programmet kjører. Når programmet avsluttes, vil filen automatisk bli slettet fra systemet.

Det finnes også en rekke andre funksjoner for å håndtere midlertidige filer i Elixir, som for eksempel `Tempfile.delete()` for å manuelt slette en fil, eller `Tempfile.get()` for å hente informasjon om en eksisterende midlertidig fil.

## Se også

- [Elixir File modulen](https://hexdocs.pm/elixir/File.html)
- [HTTPoison biblioteket](https://hexdocs.pm/httpoison)
- [Elixir Tempfile biblioteket](https://hexdocs.pm/tempfile)