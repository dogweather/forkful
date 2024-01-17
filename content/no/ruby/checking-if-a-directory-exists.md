---
title:                "Sjekke om en mappe eksisterer"
html_title:           "Ruby: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Nå som du har blitt en habil Ruby programmerer, er det på tide å dykke dypere inn i språket. En viktig del av programmering er å sjekke om en mappe eksisterer. Dette er nyttig når du ønsker å legge til nye filer i en eksisterende mappe eller å navigere mellom forskjellige mapper.

## Hva & Hvorfor?
Sjekking av mappeeksistens er en måte å kontrollere om en spesifikk mappe finnes på en gitt sti. Dette er en grunnleggende funksjon i programmering, da det hjelper deg å navigere og håndtere filer mer effektivt. Ved å sjekke om en mappe eksisterer, kan du også unngå feil og unødvendig kode som kan bremse ned programmet ditt.

## Slik gjør du det:
For å sjekke om en mappe eksisterer i Ruby, bruker du `Dir.exist?()` metoden. Denne metoden tar inn en streng som parameter, som representerer stien til den aktuelle mappen. Her er et eksempel på hvordan dette kan se ut i praksis:
```Ruby
if Dir.exist?("./mappenavn")
    puts "Mappen eksisterer!"
else
    puts "Mappen finnes ikke"
end
```
Dette vil skrive ut "Mappen eksisterer!" hvis mappen med navnet "mappenavn" finnes, ellers vil den skrive ut "Mappen finnes ikke". Du kan også bruke `Dir.exist?()` i kombinasjon med andre metoder, for eksempel `Dir.entries()` for å få en liste over filer og mapper inni den gitte mappen.

## Dykk dypere:
Å sjekke om en mappe eksisterer er en grunnleggende funksjon i alle programmeringsspråk. Faktisk var det en vanlig utfordring for mange programmerere før i tiden da datamaskiner ofte ikke hadde grafiske grensesnitt, men var basert på kommandolinjen. Å sjekke om en mappe eksisterer er også en viktig del av å håndtere feil og unntak i et program. I tillegg kan du også bruke `File.exist?()` metoden for å sjekke om en spesifikk fil eksisterer, i stedet for en mappe.

## Se også:
For mer informasjon om å sjekke mappeeksistens i Ruby, sjekk ut [Ruby dokumentasjonen](https://ruby-doc.org/core-3.0.0/Dir.html#method-c-exist-3F) for `Dir.exist?()` metoden. Du kan også sjekke ut [denne YouTube videoen](https://www.youtube.com/watch?v=NTg0EXVDeRc) for en praktisk gjennomgang av å sjekke mappeeksistens i Ruby. Lykke til med programmeringen din!