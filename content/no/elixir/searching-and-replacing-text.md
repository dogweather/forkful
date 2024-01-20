---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søk og erstatting av tekst er en handling der man finner en spesifikk streng i en tekst og erstatter den med en annen. Programmere bruker det ofte til å oppdatere kode, reformatere data, og rette feil i store filer.

## Hvordan gjøre det:
Her er et enkelt eksempel på hvordan du bruker `String.replace/3` funksjonen i Elixir til å søke og erstatte tekst:

```Elixir
text = "Hei, verden!"
new_text = String.replace(text, "verden", "Norge")
IO.puts new_text
```

Koden over vil skrive ut "Hei, Norge!"

## Dypdykk
Historisk sett, søk og erstatt-funksjonalitet har vært en kjernekomponent i tekstbehandling og programmering siden tidlige dager av data. Andre alternativer til `String.replace/3` i Elixir inkluderer bruk av `Regex.replace/3` for mer komplekse tekstmanipuleringer gjennom regulære uttrykk.

I sin kjerne, `String.replace/3` funksjonen i Elixir bruker Erlang's :binary.match funksjon til å søke etter den spesifiserte strengen, og deretter erstatter den med den nye strengen. Dette betyr at denne funksjonen er svært effektiv og rask.

## Se Også
Elixir's offisielle dokumentasjon gir mer detaljer om bruk av `String.replace/3` og `Regex.replace/3`: [https://hexdocs.pm/elixir/String.html#replace/3](https://hexdocs.pm/elixir/String.html#replace/3) og [https://hexdocs.pm/elixir/Regex.html#replace/3](https://hexdocs.pm/elixir/Regex.html#replace/3). Hvis du er interessert i å lære mer om søk og erstatting i general, sjekk ut denne historien: [https://en.wikipedia.org/wiki/Search_and_replace](https://en.wikipedia.org/wiki/Search_and_replace).