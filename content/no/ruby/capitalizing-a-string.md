---
title:                "Stor bokstav i en streng"
html_title:           "Ruby: Stor bokstav i en streng"
simple_title:         "Stor bokstav i en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Innholdsfortegnelse:

## Hva & Hvorfor?
Hvis du noensinne har sett en Ruby-kode, har du kanskje lagt merke til at noen strenger har store bokstaver, mens andre ikke gjør det. Dette kalles å "kapitalisere en streng" og er en vanlig praksis blant programmerere for å gjøre koden enklere å lese og forstå.

## Hvordan:
```
navn = "lisa"
puts navn.upcase
```
Dette vil gi følgende utgang:
```
LISA
```
I dette eksemplet har vi brukt #upcase-metoden for å konvertere strengen "lisa" til store bokstaver og deretter skrivd den ut ved hjelp av puts-kommandoen.

## Dykk dypere:
Det er flere grunner til at programmerere velger å kapitalisere strenger i kode. En av de viktigste er at det gjør det enklere å skille mellom variabler og metoder, noe som er spesielt nyttig når man jobber med store kodeprosjekter. 

Det finnes også alternative måter å kapitalisere strenger på, som for eksempel å bruke metodene #capitalize eller #titleize. Disse metodene vil kapitalisere den første bokstaven i en streng eller alle ord i en streng, henholdsvis.

I tillegg er det viktig å merke seg at noen språk, som for eksempel Java, krever at alle variabelnavn skal skrives med små bokstaver, mens andre, som for eksempel C++, tillater både store og små bokstaver. Derfor kan det være viktig å følge en konvensjon for kapitalisering i koden din, avhengig av hvilket programmeringsspråk du bruker.

## Se også:
- [Offisiell Ruby-dokumentasjon om strenger](https://ruby-doc.org/core-3.0.1/String.html)
- [Mer om kapitalisering i koden](https://www.ruby-forum.com/t/titleize/112975)
- [Diskusjon om variabelnavngivning i Java](https://stackoverflow.com/questions/3414388/java-variable-naming-convention-using-all-capital-letters-for-a-variable-name)