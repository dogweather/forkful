---
title:                "Få dagens dato"
html_title:           "Ruby: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Henting av dagens dato er en vanlig oppgave for programmerere, og det er alltid viktig å ha en nøyaktig tidsreferanse for å håndtere og organisere data. Å få den aktuelle datoen er enkelt og kan brukes til en rekke formål i programmering, som å sjekke tidspunktet for en spesifikk hendelse eller opprette en automatisk rapport.

## Hvordan:
For å få den nåværende datoen i Ruby, kan du bruke `Date.today` -metoden. Dette vil returnere en `Date` -objekt som inneholder den aktuelle datoen. Du kan også bruke `Time.now` -metoden, som vil gi deg en `Time` -objekt som inkluderer både dato og tid.

```Ruby
puts Date.today
#=> 2020-09-28

puts Time.now
#=> 2020-09-28 10:00:00 +0200
```

## Dypdykk:
Henting av dagens dato kan spores tilbake til 1960-tallet, da datamaskiner begynte å inkludere klokker for å holde styr på tiden. I tillegg til å bruke innebygde Ruby-metoder, kan du også bruke tredjepartsbiblioteker som `Date.today` og `DateTime.now` for mer avanserte funksjoner.

## Se Også:
For mer informasjon om henting av dagens dato, sjekk ut disse ressursene:

- [Ruby DateTime Dokumentasjon](https://ruby-doc.org/stdlib-2.3.0/libdoc/date/rdoc/DateTime.html)
- [Ruby Date og Time Dokumentasjon](https://ruby-doc.org/core-2.3.0/Time.html)