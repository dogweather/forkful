---
title:                "Lese en tekstfil"
html_title:           "Elixir: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

For de som er ny til programmering, kan det å lese en tekstfil virke som en enkel oppgave. Men i Elixir, som er et funksjonelt programmeringsspråk med sterkt fokus på skalerbarhet og feiltoleranse, kan det å lese en tekstfil være litt mer kompleks. I denne artikkelen vil vi dykke inn i hvordan man kan lese en tekstfil i Elixir og se på hvorfor dette kan være nyttig.

## Hvordan

For å lese en tekstfil i Elixir, kan vi bruke funksjonen `File.stream!` som åpner en strøm av data fra filen. Vi kan deretter bruke `Enum.each` for å iterere gjennom hver linje i filen og utføre ønsket handling på den. La oss se på et eksempel hvor vi leser en tekstfil som inneholder en liste over navn og skriver dem ut til konsollen:

```elixir
File.stream!("navneliste.txt")
|> Enum.each(fn line ->
  IO.puts(line)
end)
```
**Output:**
```
Per
Kari
Ole
Lise
```
Vi kan også bruke `File.read!` for å lese hele teksten i filen som en streng og deretter jobbe med den etter behov. La oss si at vi vil sjekke om navnet "Lise" finnes i filen vår:

```elixir
text = File.read!("navneliste.txt")

if String.contains?(text, "Lise") do
  IO.puts("Lise finnes i filen!")
end
```

**Output:**
```
Lise finnes i filen!
```

## Deep Dive

Nå som vi har sett hvordan man kan lese en tekstfil i Elixir ved hjelp av `File.stream!` og `File.read!`, la oss se på noen ekstra funksjoner som kan være nyttige. Vi kan bruke `File.read!("navneliste.txt", [:trim_trailing, :binary])` for å sikre at teksten vi får tilbake ikke har noen ekstra linjeskift, og at den er i binærformat. Dette kan være nyttig hvis vi skal jobbe med dataene videre.

Vi kan også bruke `File.open!` hvis vi trenger mer kontroll over lesingen av filen vår. Denne funksjonen tar en liste av opsjoner som argument, og vi kan spesifisere ting som filmodus, bufferstørrelse og datakodning. Dette kan være nyttig hvis vi for eksempel trenger å lese store filer og ønsker å optimalisere lesingen.

## Se også

- [Offisiell Elixir dokumentasjon om filbehandling](https://hexdocs.pm/elixir/File.html)
- [Elixir School: File IO](https://elixirschool.com/en/lessons/basics/io/#reading-a-file)