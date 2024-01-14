---
title:    "Ruby: Å få gjeldende dato"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er en viktig del av å programmere fordi det gir deg mulighet til å få nøyaktig informasjon om tiden og datoene for ulike handlinger.

## Hvordan

For å få den nåværende datoen i Ruby, kan du bruke Time.now metoden. Denne metoden returnerer et objekt som inneholder informasjon om den nåværende datoen og tiden. Her er et eksempel på hvordan du kan bruke det i en metode og printe ut datoen:

```Ruby
def print_current_date
  date = Time.now
  puts "I dag er det #{date.day}.#{date.month}.#{date.year}."
end

print_current_date
```

Dette vil produsere følgende output:

```
I dag er det 22.11.2021.
```

Det er også mulig å formatere datoen på forskjellige måter ved hjelp av strftime metoden. Her er et eksempel på hvordan du kan gjøre det:

```Ruby
date = Time.now
puts "I dag er det #{date.strftime("%A, %d %B %Y")}."
```

Dette vil produsere følgende output:

```
I dag er det mandag, 22 november 2021.
```

## Dypdykk

Time.now metoden bruker systemets klokke for å få den nåværende datoen og tiden. Dette betyr at datoen og tiden kan variere avhengig av hva systemet ditt er satt til. Du kan også spesifisere et annet tidssone ved å bruke Time.zone metoden.

Det er også verdt å nevne at du kan få mer spesifikk informasjon om datoen og tiden ved å bruke ulike metoder tilgjengelig i Time objektet. For eksempel kan du bruke .year, .month, .day, .hour, .min, .sec for å få individuelle deler av datoen og tiden.

## Se også

For mer informasjon om å få den nåværende datoen i Ruby, kan du se på følgende ressurser:

- [Ruby Time.now dokumentasjon](https://ruby-doc.org/core-2.7.4/Time.html#method-i-now)
- [Ruby strftime dokumentasjon](https://ruby-doc.org/core-2.7.4/Time.html#method-i-strftime)
- [Ruby Time.zone dokumentasjon](https://ruby-doc.org/core-2.7.4/Time.html#method-i-zone)
- [Ruby datetime Objekt](https://ruby-doc.org/stdlib-2.7.4/libdoc/date/rdoc/DateTime.html)