---
title:                "Ruby: Konvertere en dato til en streng"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å konvertere en dato til en streng er ofte nødvendig når man arbeider med datoer i en Ruby-applikasjon. Dette kan være nyttig for å vise datoer på en mer leselig måte eller for å lagre datoer på en spesifikk måte.

## Hvordan
Det er flere måter å konvertere en dato til en streng på i Ruby. En av de vanligste er å bruke metoden `strftime` som står for "string format time". Denne metoden tar inn et strengformat som argument og returnerer den konverterte datoen. La oss se på et eksempel:

```Ruby
date = Date.today
puts date.strftime("%d-%m-%Y")
```
Output: 02-07-2021

Her konverterer vi dagens dato til en streng på formatet "dd-mm-åååå". Du kan også bruke metoden `to_s` som konverterer datoen til en standard strengformat:

```Ruby
date = Date.new(2021, 12, 25)
puts date.to_s
```
Output: 2021-12-25

Å konvertere en dato til en streng kan også gjøres ved hjelp av interpolasjon, hvor vi bruker vitkårlige formateringsstrenger for å lage den ønskede strengen:

```Ruby
date = Date.new(2021, 6, 15)
puts "Datoen er #{date.month}-#{date.day}-#{date.year}"
```
Output: Datoen er 6-15-2021

## Dypdykk
Når vi bruker `strftime` metoden for å konvertere en dato til en streng, kan vi være mer spesifikke med hvilken informasjon vi vil ha i strengen. For eksempel kan vi bruke `%a` for å få ukedagen som en forkortet streng, eller `%B` for å få måneden som en hel streng. Det finnes mange forskjellige formateringsstrenger å velge mellom, og du kan se en full liste i [Ruby dokumentasjonen](https://ruby-doc.org/core-3.0.2/Time/strftime.html).

Det er også verdt å nevne at `strftime` er ikke begrenset til bare datoen, den kan også brukes på tidsobjekter. Du kan for eksempel skrive `Time.now.strftime("%H:%M")` for å få dagens klokkeslett som en streng.

## Se også
- [Ruby dokumentasjon - Tid og dato klasser](https://ruby-doc.org/core-3.0.2/Time.html)
- [Artikkel: Dato og tid i Ruby](https://code.tutsplus.com/tutorials/dates-and-times-in-ruby--cms-26435)
- [Video: Konvertering av datoer og tider i Ruby](https://www.youtube.com/watch?v=iXSvKYyeauE)