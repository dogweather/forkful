---
title:                "Ruby: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Konvertering av datoer til strenger er en vanlig oppgave i Ruby-programmering. Dette er fordi strenger er mye mer fleksible når det kommer til formatering og visning av datoer. Det gjør det også enklere å manipulere og sammenligne datoer i koden.

## Hvordan konvertere dato til streng i Ruby

For å konvertere en dato til en streng i Ruby, kan du bruke metoden `strftime`. Dette står for "String Format Time". La oss se på et eksempel:

```Ruby
date = Time.now # Oppretter et objekt av aktuell tid
date_string = date.strftime("%d.%m.%Y") # Konverterer dato til ønsket strengformat
puts date_string # Printer ut strengen
```

Output:

`29.03.2021`

Her ser du at vi har brukt `%d` for å vise dagen, `%m` for måned og `%Y` for år. Det finnes en hel liste med ulike symboler du kan bruke for å formatere dato og tid i Ruby. Her er noen av de vanligste:

- `%d` - Dag i måneden (1-31)
- `%m` - Måned (1-12)
- `%Y` - År (f.eks. 2021)
- `%H` - Time i 24-timers format (00-23)
- `%M` - Minutter
- `%S` - Sekunder

Du kan også kombinere disse symbolene på ulike måter for å lage ditt eget unike strengformat.

## Dykk dypere

For å ta en dypere titt på konvertering av datoer til strenger, kan vi se på hvordan det fungerer i detalj. Ruby har flere interne metoder som brukes for å håndtere dato og tid, og disse kan også brukes for å konvertere datoer til strenger. Her er noen av de viktigste metodene:

- `time.to_s` - Konverterer et tidspunkt til en streng
- `time.strftime(format)` - Konverterer et tidspunkt til en streng med det spesifiserte formatet
- `time.utc` - Returnerer tidspunktet som UTC
- `localtime` - Returnerer tidspunktet for den lokale tidssonen

Når du konverterer en dato til en streng, vil Ruby bruke standardformatet for den lokale tidssonen din. Hvis du vil konvertere til en annen tidssone, kan du bruke `strftime`-metoden og spesifisere tidssonen som en ekstra parameter.

## Se også

- [Ruby dokumentasjon om `strftime`-metoden](https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime)
- [Guide til dato og tid manipulasjon i Ruby](https://www.rubyguides.com/2015/06/ruby-time/)
- [Ruby on Rails API dokumentasjon om håndtering av dato og tid](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)