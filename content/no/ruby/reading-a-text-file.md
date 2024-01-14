---
title:    "Ruby: 'Lese en tekstfil'"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lese en tekstfil i Ruby kan være svært nyttig når man ønsker å behandle store mengder tekstbasert data. Dette kan være nyttig for å lage rapporter, analysere data eller generelt manipulere tekst på en effektiv måte. Ved å lære hvordan man kan lese en tekstfil i Ruby, utvider man sine muligheter for å jobbe med tekstbaserte oppgaver.

## Hvordan gjøre det

For å lese en tekstfil i Ruby, kan man bruke metoden `File.readlines()`. Dette vil lese hver linje i tekstfilen og returnere en liste med hver linje som et element. På denne måten kan man enkelt iterere gjennom listen og behandle hver linje separat.

```Ruby
file = File.readlines("tekstfil.txt") # Åpner tekstfilen og lagrer den som en liste

file.each do |line| # Itererer gjennom hver linje i listen
  puts line # Skriver ut linjen til skjermen
end
```

Dette er en enkel måte å lese en tekstfil på, men det finnes også andre metoder for å lese filer i Ruby. For eksempel kan man bruke `File.open()` for å åpne en fil, eller `file.gets()` for å lese en linje av gangen.

## Dypdykk

Når man leser en tekstfil i Ruby, er det viktig å være klar over hvordan filen er strukturert. Det kan være nyttig å bruke metoder som `String.chomp()` for å fjerne linjeskift fra hver linje, eller `String.split()` for å behandle hver linje som en liste med ord.

Man kan også lese tekstfiler fra eksterne kilder, for eksempel en nettside eller en database, ved hjelp av Ruby Gems som Mechanize eller Nokogiri.

## Se også

For mer informasjon om å lese og behandle tekstfiler i Ruby, kan du sjekke ut følgende ressurser:

- [Ruby Filbehandling](https://ruby-doc.org/core-2.6/File.html)
- [Ruby Gems Dokumentasjon](https://rubygems.org)
- [Mechanize Gem](https://github.com/sparklemotion/mechanize)
- [Nokogiri Gem](https://github.com/sparklemotion/nokogiri)

Lykke til med å utforske mulighetene for å lese tekstfiler i Ruby!