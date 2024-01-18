---
title:                "Analysering av dato fra en streng"
html_title:           "Arduino: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av datoer fra en tekststreng er en vanlig programmeringsoppgave som innebærer å konvertere et tekstformatert dato til en datotype som er leselig for en datamaskin. Dette er nyttig når man trenger å behandle eller lagre datoer i et program.

## Slik gjør du det:
Et eksempel på hvordan du kan parse en dato fra en tekststreng i Arduino er ved hjelp av funksjonen ```toCharArray()```, som konverterer en string til en karakter-array. Du kan deretter bruke funksjonen ```atoi()``` til å konvertere hvert tegn til et tall og bygge opp en ```Date```-variabel. Her er et enkelt eksempel:

```
String datoStreng = "20/03/2021";
char array[12];
datoStreng.toCharArray(array, 12);
int dag = atoi(array);      // Dag = 20
int måned = atoi(array + 3);  // Måned = 03
int år = atoi(array + 6);    // År = 2021
Date dato(dag, måned, år);  // Opprett en dato-variabel med verdier

```

## Dypdykk:
Parsing av datoer har vært en utfordring for programmerere siden tidlig på 1970-tallet, da de første dataprogrammene ble utviklet. Det finnes flere forskjellige metoder for å parse datoer fra en tekststreng, for eksempel ved hjelp av regulære uttrykk eller ved å splitte tekststrengen basert på et fast mønster. Det er viktig å huske på at datoformater kan variere fra land til land, og derfor bør man alltid grundig teste parsing-koden sin.

## Se også:
- [Official Arduino Reference - String toCharArray()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tochararray/)
- [Official Arduino Reference - String atoi()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/atoi/)
- [Tutorialspoint - C++ String toCharArray()](https://www.tutorialspoint.com/cplusplus/cpp_string_tochararray.htm)
- [GeeksforGeeks - How to convert string to array in C++](https://www.geeksforgeeks.org/how-to-convert-string-to-array-in-cpp/)