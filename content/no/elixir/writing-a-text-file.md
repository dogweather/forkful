---
title:                "Skriver en tekstfil"
html_title:           "Elixir: Skriver en tekstfil"
simple_title:         "Skriver en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å skrive en tekstfil er en viktig del av programmering i Elixir. Det lar deg lagre data og konfigurasjonsinnstillinger som kan brukes på tvers av forskjellige applikasjoner.

## Hvordan

For å skrive en tekstfil i Elixir, først må du åpne en fil ved hjelp av File.open/2 funksjonen. Deretter kan du bruke File.write/2 funksjonen til å skrive tekst til filen. For eksempel:

```Elixir
file = File.open("min_fil.txt", [:utf8, :write])
File.write(file, "Dette er en tekstfil skrevet med Elixir!")
```

Dette vil skrive teksten "Dette er en tekstfil skrevet med Elixir!" til filen "min_fil.txt". Husk å lukke filen når du er ferdig ved hjelp av File.close/1 funksjonen.

## Dypdykk

Når du skriver en tekstfil i Elixir, er det viktig å huske at du må håndtere eventuelle feil som kan oppstå. Det kan også være nyttig å bruke File.write!/2 funksjonen i stedet for File.write/2 for å sikre at teksten blir skrevet riktig uten feil.

Se også:

- [File modulen i Elixir Dokumentasjon](https://hexdocs.pm/elixir/File.html)
- [Elixir filbehandling i praksis fra Elixirforumet](https://elixirforum.com/t/file-handling-in-elixir/663)