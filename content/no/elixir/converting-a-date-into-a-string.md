---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en dato til en streng innebærer å endre formatet fra dato til tekst. Dette er nyttig for å gjøre datoene leselige for mennesker eller for å formatere dem for visse databaser.

## Hvordan gjøre:
Her er et eksempel på hvordan du konverterer en dato til en streng ved hjelp av Date.to_string/1 funksjonen i Elixir:

```Elixir
dato = ~D[2023-12-31]
IO.puts Date.to_string(dato)
```

Output vil være: `2023-12-31`

Her er et eksempel på bruk av strftime funksjonen for mer spesifikk formatering:

```Elixir
dato_til_streng = Timex.format!(~N[2023-12-31T00:00:00], "{YYYY}-{0M}-{0D}", :strftime)
IO.puts dato_til_streng
```

Output vil være: `2023-12-31`

## Dyp Dykk
Utviklingen av datasystemer har resultert i ulike metoder for å representere datoer som strenger. Elixir, inspirert av Erlang og Ruby, tilbyr brukervennlighet i dette aspektet.

Alternativene for å konvertere datoer til strenger inkluderer bruk av innebygde funksjoner som Date.to_string/1 eller ved å bruke eksterne biblioteker som Timex for mer komplekse behov.

Implementeringsdetaljene dreier seg stort sett om å forstå formatene vi bruker i funksjonene. For eksempel i strftime funksjonen, der `{YYYY}`, `{0M}`, og `{0D}` representerer år, måned og dag.

## Se Også
For mer informasjon, sjekk ut følgende:

- [Date module official documentation](https://hexdocs.pm/elixir/Date.html)
- [Timex library on GitHub](https://github.com/bitwalker/timex)