---
title:                "Ruby: Sjekke om en mappe eksisterer"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man programmerer i Ruby, kan man ofte møte på situasjoner der man må sjekke om en mappe eksisterer. Dette kan være nyttig for å unngå feil i koden, eller for å sikre at programmet fungerer som forventet.

## Slik gjør du det

Sjekking av mapper i Ruby er enkelt og kan gjøres ved hjelp av følgende kode:

```Ruby
if Dir.exist?('mappe_navn')
  puts "Mappen eksisterer"
else
  puts "Mappen eksisterer ikke"
end
```

Koden over sjekker om en mappe med navnet "mappe_navn" eksisterer. Dersom den gjør det, vil det bli printet ut en melding som sier "Mappen eksisterer". Hvis ikke, vil det bli printet ut en melding som sier "Mappen eksisterer ikke".

Man kan også bruke denne koden til å sjekke om en fil eksisterer, ved å bytte ut "Dir.exist?" med "File.exist?".

## Dypdykk

Når man sjekker om en mappe eksisterer, kan det være nyttig å vite hvordan man håndterer tilfeller der mappen ikke finnes. I tillegg kan man bruke andre metoder som "Dir.empty?" for å sjekke om mappen er tom.

Det kan også være lurt å inkludere feilhåndtering i koden. Dersom man forventer at mappen skal være tilstede, kan man bruke "begin/rescue" for å håndtere eventuelle feil som kan oppstå.

## Se også

- [Ruby dokumentasjon for Dir](https://ruby-doc.org/core-2.7.2/Dir.html)
- [Ruby dokumentasjon for File](https://ruby-doc.org/core-2.7.2/File.html)
- [Enkel feilhåndtering i Ruby](https://www.rubyguides.com/2018/06/ruby-exceptions/)