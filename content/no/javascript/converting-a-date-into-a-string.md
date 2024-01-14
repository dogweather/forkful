---
title:                "Javascript: Oversette en dato til en streng"
simple_title:         "Oversette en dato til en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til at man vil konvertere en dato til en tekstlig representasjon i et Javascript-program. Dette kan være nyttig for å vise datoer i en mer leselig form for brukere, eller for å lagre datoer i en mer strukturert format.

## Hvordan

For å konvertere en dato til en tekstlig representasjon, kan du bruke innebygde funksjoner i Javascript. Her er et eksempel på hvordan du kan gjøre dette:

```Javascript
// Opprett en variabel med dagens dato
var today = new Date();

// Bruk toString()-funksjonen for å konvertere datoen til en tekstlig representasjon
var dateAsString = today.toString();

// Skriv ut den konverterte datoen i konsollen
console.log(dateAsString);

// Dette vil gi følgende output:
// Wed Jun 24 2020 14:36:55 GMT+0200 (Central European Summer Time)
```

Som du kan se i eksempelet over, brukte vi funksjonen `toString()` for å konvertere datoen til en tekstlig representasjon. Dette er den enkleste måten å gjøre det på, men det finnes også andre metoder for å konvertere datoer til tekst i Javascript. Det er viktig å merke seg at formateringen av den tekstlige representasjonen vil variere avhengig av språk og region.

## Dykk ned

Det finnes mange forskjellige formater som man kan bruke for å konvertere en dato til en tekstlig representasjon i Javascript. Dette kan inkludere å legge til klokkeslett, dager i uka eller måneder, og mye mer. Du kan også bruke forskjellige funksjoner for å formatere datoen på en mer spesifikk måte.

Her er et annet eksempel på hvordan du kan konvertere en dato til et spesifikt format:

```Javascript
// Opprett en ny variabel med dagens dato
var today = new Date();

// Bruk de innebygde funksjonene for å hente ut dag, måned og år
var day = today.getDate();
var month = today.getMonth() + 1;
var year = today.getFullYear();

// Kombiner verdiene for å danne en tekstlig representasjon i ønsket format
var dateAsString = day + "-" + month + "-" + year;

// Skriv ut den konverterte datoen i konsollen
console.log(dateAsString);

// Dette vil gi følgende output:
// 24-6-2020
```

Som du kan se, kan du tilpasse formatet på den tekstlige representasjonen basert på dine behov og preferanser.

## Se også

* [Javascript Date Objekt](https://www.w3schools.com/jsref/jsref_obj_date.asp)
* [toString() metode](https://www.w3schools.com/jsref/jsref_tostring_date.asp)
* [Date Format Library](http://blog.stevenlevithan.com/archives/date-time-format)