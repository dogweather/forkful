---
title:    "Ruby: Å beregne en dato i fremtiden eller fortiden"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nødvendig å beregne en dato i fremtiden eller fortiden i et Ruby-program. Dette kan være for å planlegge hendelser, for å lage en kalenderfunksjon eller for å utføre beregninger med datoer. Ved å forstå hvordan man kan beregne datoer i Ruby, kan du gjøre programmene dine mer dynamiske og funksjonelle.

## Hvordan

Det er flere måter å beregne en dato i Ruby, avhengig av hvilken versjon av Ruby du bruker og hva du ønsker å oppnå. Her er et eksempel på hvordan du kan beregne en dato i fremtiden eller fortiden ved hjelp av Ruby-kode:

```Ruby
# importerer Date-modulen
require 'date'

# beregner dagens dato
today = Date.today

# legger til 7 dager til dagens dato
future_date = today + 7 

# trekker fra 7 dager fra dagens dato
past_date = today - 7 

# skriver ut resultatene
puts "Dagens dato: #{today}"
puts "Dato i fremtiden: #{future_date}"
puts "Dato i fortiden: #{past_date}"
```

Dette vil gi følgende utskrift:

```
Dagens dato: 2021-07-21
Dato i fremtiden: 2021-07-28
Dato i fortiden: 2021-07-14
```

Som du kan se, kan vi enkelt legge til eller trekke fra et gitt antall dager fra en dato ved hjelp av "+" og "-" operatørene. Det er også verdt å merke seg at Ruby har innebygde metoder for å beregne datoer basert på andre enheter som uker, måneder og år.

## Dypdykk

Det er viktig å merke seg at datoer i Ruby er objekter og har mange nyttige metoder som kan brukes til å utføre ulike operasjoner. For eksempel kan du bruke `strftime`-metoden til å formatere en dato på en spesifikk måte. Her er et eksempel:

```Ruby
my_date = Date.parse("2021-07-21")
puts my_date.strftime("%d.%m.%Y")
```

Dette vil resultere i følgende utskrift:

```
21.07.2021
```

Det finnes også mange tilleggsmoduler som kan hjelpe deg med å håndtere datoer i Ruby, som for eksempel `chronic`-modulen for å arbeide med naturlige språk og `business_time`-modulen for å håndtere forretningsdager.

## Se Også

- [Ruby Date dokumentasjon](https://ruby-doc.org/stdlib-3.0.1/libdoc/date/rdoc/Date.html)
- [Chronic-modulen](https://github.com/mojombo/chronic)
- [Business Time-modulen](https://github.com/bokmann/business_time)