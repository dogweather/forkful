---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Dato-til-streng-konvertering i Ruby henviser til prosessen med å transformere en datoobjekt til en streng. Dette lar programmerere vise datoer i mer lesbare eller tilpassede formater.

## Hvordan:

Å konvertere en dato til en streng i Ruby er enkelt og greit. Kodeblokken nedenfor illustrerer grunnleggende bruk:

```Ruby
# importer Ruby's innebygde dato-bibliotek
require 'date'

# Opprett en dato-objekt
d = Date.new(2021, 2, 3)

# Konverter dato-objektet til en streng
s = d.to_s

puts s
```

I dette eksemplet vil output være "2021-02-03".

## Dyp Dykk

Dato-til-streng-konvertering har blitt et uunnværlig verktøy fordi det gjør dataene mer robuste og allsidige. Innføringen av streng-datoer kan spores tilbake til tidenes opprinnelse for databaser og dataprogrammering, og metoden har blitt bevart på grunn av dens ekstreme brukbarhet.

I Ruby har vi alternative metoder som `strftime` for å tilpasse hvordan vi vil datoene skal se ut.

```Ruby
d = Date.new(2021, 2, 3)

s = d.strftime("%B %d, %Y")

puts s
```

Her ville resultatet være "February 03, 2021".

Implementeringsdetaljer for disse konverteringene finnes i Ruby-dokumentasjonen. I det store og hele er de ganske enkle å bruke, men det er viktig å merke seg at strengene som produseres alltid vil være avhengige av datoen som brukes til å lage dem.

## Se Også:

- [Ruby Dato-Dokumentasjon](http://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Ruby Strftime-Dokumentasjon](https://apidock.com/ruby/Date/strftime)

Forsikre deg om å utforske disse lenkene for mer detaljert informasjon.