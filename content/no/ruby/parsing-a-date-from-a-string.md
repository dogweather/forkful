---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing en dato fra en streng betyr å konvertere tekst til datatypen dato. Programmerere gjør dette for å håndtere, lagre og manipulere dataene mer effektivt.

## Hvordan:
Her er noen enkle trinn for å parse en dato fra en streng i Ruby:

```Ruby
require 'date'

dato_streng = "2022-3-17"
dato = Date.parse(dato_streng)

puts dato
```

Når du kjører programmet over, vil utskriften være:

```
2022-03-17
```

## Dyp Dykk:
(1) Historisk Kontekst: I Ruby, dato parsing har vært en del av standard biblioteket siden starten av språket. Det er det fordi datoer og tidspunkter er en stor del av moderne programmering.

(2) Alternativer: Du kan også bruke DateTime.parse hvis du også trenger tidspunktet. For eksempel:

```Ruby
require 'date'

datetime_string = "2022-03-17T15:30:00"
datetime = DateTime.parse(datetime_string)

puts datetime
```

Utskriften vil være:

```
2022-03-17T15:30:00+00:00
```

(3) Implementeringsdetaljer: I Ruby, både `Date.parse` og `DateTime.parse` bruker den samme parsing algoritmen. De prøver å finne en dato i strengen med mest mulig nøyaktighet. Hvis parsing feiler, vil de kaste en ArgumentError.

## Se Også:

[Date Class Documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)

[DateTime Class Documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)

[Dato og Tids Håndtering i Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)