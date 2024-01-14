---
title:                "C++: Sammenligne to datoer"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

Denne bloggen vil lære deg hvordan du kan sammenligne to datoer ved hjelp av C++ programmeringsspråket. Selv om dette kan virke som en enkel oppgave, er det nyttig å forstå hvordan dette fungerer for å kunne håndtere datoer på en mer avansert måte i fremtidige prosjekter.

## Hvorfor

Å sammenligne to datoer kan være nyttig for å sortere eller filtrere data etter dato. Dette kan være spesielt viktig i finansbransjen eller ved bruk av tidssensitive data. Derfor er det viktig å kunne håndtere datoer på en effektiv måte ved å sammenligne dem.

## Hvordan gjøre det

Vi vil nå se på to datoer som skal sammenlignes, "dato1" og "dato2". Først må vi definere disse datoene ved å bruke "tm" structen som er innebygd i C++.

```C++
tm dato1 = { 0 };
tm dato2 = { 0 };
```

Deretter må vi fylle ut verdier for dag, måned og år for hver dato. Dette kan gjøres ved hjelp av structens variabler "tm_mday" for dag, "tm_mon" for måned og "tm_year" for år. Det må også legges til 1900 i år-variabelen for å få den korrekte datoen.

```C++
dato1.tm_mday = 10;
dato1.tm_mon = 8;
dato1.tm_year = 2021 - 1900;

dato2.tm_mday = 15;
dato2.tm_mon = 8;
dato2.tm_year = 2021 - 1900;
```

Nå kan vi sammenligne disse to datoene ved å bruke funksjonen "difftime" som returnerer antall sekunder mellom to datoer. Dette kan uttrykkes i dager ved å dele antallet sekunder på 86400 (24 timer * 60 min * 60 sek).

```C++
int diff = difftime(mktime(&dato1), mktime(&dato2)) / 86400;

if (diff == 0) {
    cout << "Datoene er like." << endl;
} else if (diff < 0) {
    cout << "Dato1 kommer før Dato2." << endl;
} else {
    cout << "Dato2 kommer før Dato1." << endl;
}
```

Resultatet vil bli "Dato1 kommer før Dato2.", siden 10. august kommer før 15. august.

## Dypdykk

I dypdykket vil vi se på hvordan "difftime" funksjonen fungerer. Denne funksjonen returnerer antall sekunder fra to gitt datoer, uttrykt som et "double" tall. Det er derfor viktig å konvertere dette til ønsket tidsenhet, som i vårt tilfelle var dager.

Vi bruker også funksjonen "mktime" som konverterer "tm" structen til en tidsvariabel som kan håndteres av "difftime" funksjonen.

## Se Også

- [C++ Reference - difftime](https://www.cplusplus.com/reference/ctime/difftime/)
- [C++ Reference - mktime](https://www.cplusplus.com/reference/ctime/mktime/)