---
title:                "Ruby: Å få gjeldende dato"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å vite den nåværende datoen er en viktig del av å skrive effektiv kode. Det kan hjelpe deg med å lage dynamiske programmer, spore tidsfølsomme hendelser og holde orden på tidsfrister.

## Hvordan få den nåværende datoen i Ruby

Det er flere måter å få den nåværende datoen i Ruby på, men den enkleste er å bruke `Date`-klassen og metoden `today`. Her er et eksempel på hvordan du kan gjøre det:

```Ruby
require 'date'

current_date = Date.today

puts current_date
```

Når du kjører dette kodesnippet, vil du få utskrift av den nåværende datoen i formatet `yyyy-mm-dd`.

Du kan også formatere datoen ved å bruke metoden `strftime`:

```Ruby
formatted_date = current_date.strftime("%d %b %Y")

puts formatted_date
```

Dette vil gi deg utskrift av datoen i formatet `dd mmm yyyy` (for eksempel 31 Dec 2020).

Det er også verdt å nevne at du også kan få den nåværende datoen og tidspunktet ved å bruke `DateTime`-klassen og metoden `now` på samme måte som vi gjorde med `Date`-klassen og metoden `today`.

## Dykk dypere

Hvis du vil lære mer om hvordan Ruby håndterer datoer og tider, kan du utforske `Time`- og `DateTime`-klassene. Disse klassene har flere nyttige metoder for å konvertere datoer og tider og beregne forskjeller mellom dem.

En annen nyttig metode å vite om er `parse`, som lar deg konvertere tekststrenger til dato- eller tidobjekter. Her er et eksempel:

```Ruby
date_string = "2020-12-31"

parsed_date = Date.parse(date_string)

puts parsed_date
```

Dette vil konvertere tekststrengen til et `Date`-objekt og gi utskrift av datoen.

## Se også

- [Ruby dokumentasjon om Date-klassen](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby dokumentasjon om DateTime-klassen](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Ruby dokumentasjon om Time-klassen](https://ruby-doc.org/stdlib-2.7.2/libdoc/time/rdoc/Time.html)