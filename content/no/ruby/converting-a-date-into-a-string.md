---
title:    "Ruby: Å konvertere en dato til en streng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med å konvertere datoer i Ruby, kan du noen ganger ønske å konvertere en dato til en streng. Dette kan være nyttig for å vise datoer på en bestemt måte, for å lagre dem i en database eller for annen manipulasjon av data.

## Slik gjør du det

For å konvertere en dato til en streng i Ruby, kan du bruke `.strftime` -metoden. Dette vil ta inn et spesifikt format og returnere en strengreprsentasjon av datoen. For eksempel: 

```Ruby
d = Date.new(2020, 05, 28) # Oppretter en ny dato for 28. mai 2020 
d.strftime('%d.%m.%Y') # Konverterer datoen til streng og returnerer "28.05.2020"
```

Du kan også bruke `.to_s` -metoden, som vil konvertere datoen til standardformatering i henhold til datamaskinens locale. For eksempel:

```Ruby
d = Date.new(2020, 05, 28) # Oppretter en ny dato for 28. mai 2020 
d.to_s # Konverterer datoen til streng og returnerer "28/05/2020" hvis locale er satt til engelsk
```

## Dypdykk

Det er også mulig å bruke `.to_formatted_s` -metoden for å konvertere datoen til streng med et spesifikt format. Denne metoden tar inn et symbol som representerer det ønskede formatet. For eksempel:

```Ruby
d = Date.new(2020, 05, 28) # Oppretter en ny dato for 28. mai 2020 
d.to_formatted_s(:long) # Konverterer datoen til streng og returnerer "May 28, 2020"
```

I tillegg har Ruby en innebygd metode for å konvertere datoer til en lokal formatert streng ved hjelp av `.l` metoden. Dette vil returnere en lokal formatert streng basert på datamaskinens locale. For eksempel:

```Ruby
d = Date.new(2020, 05, 28) # Oppretter en ny dato for 28. mai 2020 
d.l # Hvis locale er satt til engelsk, returnerer det "28 May 2020"
```

## Se også

- [Ruby Dokumentasjon: Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html) 
- [Ruby Date Formatter](https://ruby-doc.org/core-2.5.0/Date.html#method-i-to_s)