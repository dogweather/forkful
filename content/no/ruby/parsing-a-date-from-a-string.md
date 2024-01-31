---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:38:15.833489-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å parse en dato fra en streng betyr å konvertere tekstformatet til et datoobjekt som Ruby forstår. Programmerere gjør dette for å enkelt kunne bearbeide og sammenligne datoer, som er kritisk for funksjoner som tidsstyring og datalogging.

## Hvordan Gjøre Det:
Her er hvordan du gjør det i Ruby:

```Ruby
require 'date'

# Ordinær parsing
date_string = "2023-04-10"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-10

# Parsing med spesifikt format
date_string_custom = "10-04-2023"
parsed_date_custom = Date.strptime(date_string_custom, "%d-%m-%Y")
puts parsed_date_custom
# => 2023-04-10
```

`Date.parse` tolker de fleste datostrengformerater. Bruk `Date.strptime` hvis du har et spesifikt format som må følges.

## Dypdykk
Tilbake i de gode gamle dager måtte programmerere ofte skrive sin egen dato-parsing-logikk. Heldigvis innførte Ruby biblioteket `date`, som gjør det til en smal sak.

Det finnes alternativer til innebygd Ruby-biblioteket `date`:
- `Time` klassen for tidsstempler.
- Eksterne gems som `Chronic` for naturlig språkparsing.

Implementasjonsdetaljer:
- `Date.parse` er smidig, men kan feiltolke d/m/Y og m/d/Y.
- `Date.strptime` krever at du kjenner formatet på forhånd.
- Ruby håndterer godt tidssone-konvertering når det er nødvendig.

## Se Også
- Ruby's Date dokumentasjon: https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html
- Tutorial om `Time` klassen i Ruby: https://www.tutorialspoint.com/ruby/ruby_time_date.htm
- Chronic gem dokumentasjon for naturlig språkparsing: https://github.com/mojombo/chronic
