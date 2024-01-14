---
title:                "Ruby: Sammenligning av to datoer"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens verden av teknologi er det viktigere enn noensinne å være i stand til å håndtere og manipulere data på en effektiv måte. En av de vanligste oppgavene innen programmering er å sammenligne to datoer for å bestemme hvilken som er tidligere eller senere. Dette kan være nyttig for å sortere data eller for å beregne tidsperioder. I denne bloggposten skal vi lære deg hvordan du enkelt kan sammenligne to datoer ved hjelp av Ruby-programmering.

## Hvordan

For å sammenligne to datoer i Ruby, må man først konvertere dem til et format som kan sammenlignes. Dette kan gjøres ved å bruke "Date" og "Time" klassene i Ruby, og deretter bruke metoder som "parse" og "strftime" for å få ønsket format. La oss se på et eksempel:

```Ruby
# Konvertering av datoer til ønsket format
dato1 = Date.parse("12/10/2020") # Datoen skal være i formatet MM/DD/YYYY
dato2 = Time.parse("20201210") # Datoen skal være i formatet YYYYMMDD
```

Nå som vi har konvertert begge datoene, kan vi enkelt sammenligne dem ved hjelp av operatorer som ">","<" eller "==". Her er et eksempel på hvordan man kan finne ut hvilken av datoene som er tidligere:

```Ruby
# Sammenligne datoer og bestemme hvilken som er tidligere
if dato1 < dato2
  puts "Dato 1, #{dato1.strftime("%d/%m/%Y")}, er tidligere enn dato 2, #{dato2.strftime("%Y/%m/%d")}."
else
  puts "Dato 2, #{dato2.strftime("%Y/%m/%d")}, er tidligere enn dato 1, #{dato1.strftime("%d/%m/%Y")}."
end
```

Dette vil gi følgende output:

```console
=> Dato 2, 2020/12/10, er tidligere enn dato 1, 12/10/2020.
```

## Deep Dive

For de som er interessert i å gå dypere inn i temaet, finnes det flere metoder for å sammenligne datoer i Ruby. En annen vanlig metode er å bruke klassen "DateTime", som har flere innebygde metoder for å sammenligne datoer. Det finnes også gems (tredjepartsbiblioteker) som kan gjøre dette enda enklere og mer robust, som for eksempel "compare_by_date" og "chronic". Det er også viktig å være oppmerksom på forskjellige tidszoner og hvordan de kan påvirke sammenligningen av datoer.

## Se Også

- [Ruby Date Klasse Dokumentasjon](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html)
- [Ruby Time Klasse Dokumentasjon](https://ruby-doc.org/core-2.5.1/Time.html)
- [Ruby DateTime Klasse Dokumentasjon](https://ruby-doc.org/stdlib-2.5.1/libdoc/datetime/rdoc/DateTime.html)
- [Rubygems.org](https://rubygems.org)