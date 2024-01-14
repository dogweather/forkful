---
title:    "Gleam: Sjekke om en mappe eksisterer"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor

Når vi skriver programmene våre, er det viktig å inkludere logikk for å håndtere eventuelle feil eller uventede situasjoner. En vanlig feil i programmering er å prøve å aksessere en fil eller mappe som ikke eksisterer. Dette kan føre til at programmet krasjer eller gir ugyldige resultater. Derfor er det viktig å ha en måte å sjekke om en mappe eksisterer før vi prøver å bruke den i koden vår. Gleam gir oss en enkel måte å gjøre dette på. 

## Hvordan

For å sjekke om en mappe eksisterer, kan vi bruke funksjonen `Dir.exists`. Denne funksjonen tar inn en streng som representerer stien til mappen vi vil sjekke og returnerer enten `true` eller `false` avhengig av om mappen eksisterer eller ikke.

```Gleam
let result = Dir.exists("stien/til/mappen")

if result {
  // Mappen eksisterer, vi kan fortsette med koden vår
} else {
  // Mappen eksisterer ikke, vi kan håndtere feilen her
}
```

Hvis mappen ikke eksisterer, vil funksjonen returnere `false` og vi kan håndtere dette på en hensiktsmessig måte. Vi kan også inkludere denne sjekken som en del av et større program, slik at vi unngår feil og ugyldig kode.

## Dypdykk

En interessant ting å merke seg er at `Dir.exists`-funksjonen også sjekker om brukeren har rettigheter til å aksessere mappen, ikke bare om den fysisk eksisterer. Dette kan være nyttig hvis programmet ditt krever spesifikke tilganger for å kjøre riktig.

Vi kan også kombinere `Dir.exists` med andre funksjoner i Gleam, for eksempel `Dir.list` for å få en liste over alle filer og mapper i en gitt mappe. Dette kan være nyttig for å utforske og håndtere filstrukturer i programmene våre.

## Se også

- [Gleam dokumentasjon](https://gleam.run/documentation)
- [Offisiell Gleam GitHub repository](https://github.com/gleam-lang/gleam) 
- [Gleam Community Discord server](https://discord.gg/WrfpUMF)