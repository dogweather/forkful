---
title:                "En date från en sträng"
html_title:           "C++: En date från en sträng"
simple_title:         "En date från en sträng"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parsa" ett datum från en sträng innebär att konvertera ett datum som är skrivet som en textsträng till ett formaterat datum som kan användas i ett program. Programerare gör detta för att kunna använda datum som variabler i sina program, för att kunna jämföra och manipulera datum på ett enklare sätt.

## Hur man gör:
Här är ett enkelt exempel på hur man kan parsea ett datum från en sträng i C++:

```C++
#include <iostream>
#include <ctime>

int main() {
  std::string date = "2020-07-18"; //strängen som ska parsas
  std::tm t = {}; //tom struct för att lagra datumet
  std::istringstream ss(date); //skapar en istringstream för att kunna läsa strängen
  ss >> std::get_time(&t, "%Y-%m-%d"); //parsar strängen till datumet
  if(ss.fail()){ //om parsningen misslyckas 
    std::cout << "Ogiltigt datum!\n";
  } else { //om parsningen är lyckad
    std::cout << "År: " << t.tm_year + 1900 << std::endl; //tm_year räknas från 1900
    std::cout << "Månad: " << t.tm_mon + 1 << std::endl; //tm_mon räknas från 0
    std::cout << "Dag: " << t.tm_mday << std::endl;
  }
  return 0;
}
```

Kodkommentarer är skrivna på engelska av konvention, men själva koden kan naturligtvis vara på svenska. Output av ovanstående kod skulle bli:

```2020-07-18
År: 2020
Månad: 7
Dag: 18```

## Djupdykning:
Funktionen `get_time()` som används i detta exempel är en del av standardbiblioteket `<ctime>` och introducerades i C++11. Innan dess fanns inte ett standardiserat sätt att parsa datum från strängar i C++, vilket gjorde det svårt och osäkert att göra det på ett portabelt sätt.

Det finns även andra sätt att parsea datum från strängar i C++, bland annat genom att använda bibliotek såsom Boost.Date_Time eller genom att själv implementera en parser. Men många föredrar att använda standardbibliotekets `get_time()`-funktion eftersom det är med i själva språket och därmed mindre beroende av externa bibliotek.

## Se även:
1. [Dokumentation för `get_time()`](https://en.cppreference.com/w/cpp/io/manip/get_time)
2. [Boost.Date_Time](https://www.boost.org/doc/libs/1_73_0/doc/html/date_time.html)
3. [Julian Day - ett numeriskt format för datum](https://en.wikipedia.org/wiki/Julian_day)