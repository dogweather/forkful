---
title:                "Å skrive en tekstfil"
html_title:           "Ruby: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Å skrive en tekstfil kan være nyttig av flere årsaker. Det kan være for å lagre data, lage dokumentasjon eller for å lage en liste over oppgaver. Uansett formålet, er det nyttig å kunne skrive tekstfiler ved hjelp av Ruby.

## Hvordan
Det er enkelt å skrive en tekstfil ved hjelp av Ruby. Følg disse trinnene:

1. Åpne opp en teksteditor som du foretrekker.
2. Skriv koden ```Ruby
   File.write("filnavn.txt", "tekst som skal skrives")
   ```
   Dette vil opprette en ny tekstfil med navnet "filnavn.txt" og skrive teksten som er oppgitt inne i filen.
3. For å legge til mer tekst i filen, kan du bruke ```Ruby
   File.append("filnavn.txt", "mer tekst")
   ```
   Dette vil legge til den nye teksten i eksisterende fil.
4. For å lese innholdet av en tekstfil, kan du bruke ```Ruby
   File.read("filnavn.txt")
   ```
   Dette vil returnere teksten som er skrevet inne i filen.

## Deep Dive
Når du skriver en tekstfil ved hjelp av Ruby, kan du også spesifisere hvilket tegnsett som skal brukes. Dette er nyttig hvis du for eksempel ønsker å skrive tekstfiler som skal brukes på ulike språk.

Du kan også bruke "```Ruby
File.open("filnavn.txt", "w") do |fil|
  fil.puts "tekst"
end
```"
Denne koden gjør det samme som ```Ruby File.write```, men den lukker automatisk filen etter at den er ferdig med å skrive.

## Se også
- [Offisiell Ruby-dokumentasjon for å skrive til tekstfiler](https://ruby-doc.org/core-3.0.0/File.html#method-c-open)
- [Tutorialspoint: Ruby - Filbehandling](https://www.tutorialspoint.com/ruby/ruby_file_handling.htm)
- [RubyGuides: How to Write a File in Ruby](https://www.rubyguides.com/2015/05/writing-a-file-in-ruby/)