---
title:    "Ruby: Å lese en tekstfil"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese og håndtere tekstfiler i Ruby er en viktig ferdighet for enhver programmerer. Dette kan være nyttig for å lagre og behandle store mengder data eller for å lese og analysere eksterne filer. Uansett hva formålet er, er det avgjørende å forstå hvordan man leser en tekstfil i Ruby.

## Hvordan

For å lese en tekstfil i Ruby, må vi først åpne filen ved hjelp av File-klassen og etablerer en stream for filen. Dette kan gjøres ved hjelp av "File.open" metodikk:

```ruby
file = File.open("tekstfil.txt", "r") 
```
Her "tekstfil.txt" er navnet på filen vi ønsker å lese, og "r" står for "read mode". Det er viktig å merke seg at hvis filen ikke finnes, vil det bli opprettet en ny fil med det angitte navnet.

Deretter kan vi bruke "File.foreach" metoden til å lese og behandle hver linje i filen:

```ruby
File.foreach("tekstfil.txt") { |line|
  puts line
}
```
Denne koden vil skrive ut hver linje i filen på skjermen.

For å lukke filen når vi er ferdig med å jobbe med den, kan vi bruke "file.close" metoden. Det vil sikre at ressursene til filen blir frigjort.

## Dypdykk

I tillegg til den grunnleggende koden for å lese en tekstfil, er det nyttig å kjenne til noen flere funksjoner og begreper relatert til å håndtere tekstfiler i Ruby.

En av disse er "File.readlines" metoden, som leser innholdet i filen og returnerer en array med hver linje som en verdi.

Det er også mulig å bruke "file.gets" metoden for å lese en bestemt linje i filen. For eksempel, hvis vi ønsker å lese bare den tredje linjen, kan vi bruke "file.gets[2]".

I tillegg, hvis vi vil skrive til en tekstfil, kan vi bruke "file.write" eller "file.puts" metoden. "file.write" vil skrive til filen uten å legge til en ny linje, mens "file.puts" vil legge til en ny linje etter at teksten er skrevet.

Det er viktig å merke seg at når du arbeider med tekstfiler, er det viktig å sjekke for eventuelle unntak og håndtere dem riktig for å unngå at programmet krasjer.

## Se også

- ["The Basics of Reading and Writing Files in Ruby" av Rubyguides](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [Ruby File-klassen dokumentasjon](https://ruby-doc.org/core-2.7.1/File.html)
- ["Manipulating files and directories in Ruby" av Thoughtbot](https://thoughtbot.com/blog/manipulating-files-and-directories-in-ruby)