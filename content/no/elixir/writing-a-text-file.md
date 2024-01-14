---
title:                "Elixir: Skrive en tekstfil"
simple_title:         "Skrive en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Velkommen til bloggen vår! I dag skal vi snakke om en grunnleggende, men viktig funksjonalitet i Elixir - å skrive en tekstfil. Å skrive en tekstfil kan virke som en enkel oppgave, men det kan være svært nyttig i mange programmeringsscenarier. La oss se nærmere på hvorfor.

Å skrive en tekstfil lar deg lagre data og informasjon på en organisert måte. Dette kan være alt fra en liste over brukere til resultater fra et dataanalyseprogram. Dette gir deg muligheten til å hente og bearbeide data senere. Å kunne lagre og lese data fra en tekstfil er også en grunnleggende ferdighet som er viktig å kunne for å skrive mer komplekse programmer.

## Hvordan

For å skrive en tekstfil i Elixir, må vi først åpne en ny fil ved hjelp av `File.open/2` funksjonen. Her kan vi gi filen et navn og spesifisere om vi ønsker å skrive eller lese fra filen. La oss se på et eksempel:

```
File.open("min_fil.txt", [:write], fn(file) ->
  IO.write(file, "Dette er en tekstfil skrevet med Elixir!")
end)
```

Her åpner vi en fil kalt "min_fil.txt" og spesifiserer at vi vil skrive til den ved å bruke `:write` parameter. `IO.write` funksjonen lar oss skrive en streng til filen. Etter å ha kjørt koden, vil en ny fil "min_fil.txt" bli opprettet og inneholde teksten vår.

## Dypdykk

Nå som vi har lært hvordan vi skriver en tekstfil, la oss dykke dypere inn i dette emnet. I Elixir, kan vi også endre eksisterende tekstfiler ved å bruke `:append` parameter i `File.open/2` funksjonen. Dette lar oss legge til ny informasjon til slutten av en eksisterende fil.

Vi kan også bruke `IO.gets` funksjonen til å lese en fil linje for linje. Denne funksjonen returnerer hver linje som en streng som vi kan lagre eller behandle videre. Vi kan også bruke `IO.read` for å lese hele filen som en streng.

## Se også

- [Elixir Docs: File](https://hexdocs.pm/elixir/File.html)
- [Elixir School: File Module](https://elixirschool.com/en/lessons/basics/file/)
- [Elixir Forum: Writing to a file](https://elixirforum.com/t/writing-to-a-file/4626)