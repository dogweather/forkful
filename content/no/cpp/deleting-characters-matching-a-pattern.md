---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som samsvarer med et mønster, betegner prosessen med å fjerne spesifikke tegn fra en streng basert på et definert mønster. Programmerere gjør dette for å manipulere data, korrigere feil, og forbedre databehandlingseffektivitet.

## Hvordan:
Her skal vi vise hvordan du kan slette alle forekomster av et bestemt tegn fra en streng ved hjelp av `std::remove` funksjonen i C++.

```C++
#include <iostream>   
#include <algorithm>   

int main() {
   std::string str = "Taa denne tekststringen";
   // Tegn vi ønsker å fjerne
   char c = 'a';
   
   str.erase(std::remove(str.begin(), str.end(), c), str.end());
   std::cout << str << std::endl;
   return 0;
}
```
Kjører du dette, vil output være "T denne tekstringen" siden alle 'a' i teksten har blitt fjernet.

## Dypdykk
Tidligere i programmeringshistorien, før C++11 introduserte `std::remove`, matte programmerere manuelt iterere gjennom strenger for å finne og slette spesifikke tegn. Dette var både tidkrevende og utsatt for feil.

Alternativt til `std::remove`, kan man også bruke `std::regex_replace` til å erstatte mønstre med en annen streng, som kan være tom for å slette. Denne metoden har mer fleksibilitet, men kan også være mer krevende å implementere.

Nøkkelen til hvorfor `std::remove` fungerer, ligger i STL (Standard Template Library). Funksjonen endrer ikke størrelsen på strengen. I stedet flytter den ikke-borttatte elementer mot begynnelsen av strengen, og returnerer en iterator som peker til den nye "enden". En påfølgende `erase` operasjon fjerner uønskede tegn.

## Se Også
For mer informasjon om dette emnet, sjekk ut følgende ressurser:
1. [cplusplus.com - std::remove](http://www.cplusplus.com/reference/algorithm/remove/)
2. [cppreference.com - Slett fra en streng i C ++](https://en.cppreference.com/w/cpp/string/basic_string/erase)
3. [stackoverflow.com - Hvordan slette et tegn fra en streng ved hjelp av C++](https://stackoverflow.com/questions/5891610/how-to-remove-certain-characters-from-a-string-in-c)