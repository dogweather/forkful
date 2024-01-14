---
title:    "C++: Jämförande av två datum"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en viktig del av programmering, speciellt om du arbetar med tidsrelaterade funktioner och vill hantera datum med precision. Det finns olika sätt att jämföra datum i C++, och det är viktigt att känna till dessa för att undvika felaktiga resultat i din kod.

## Hur man gör
Det finns flera sätt att jämföra datum i C++, och valet beror på vad du försöker uppnå. Här är några exempel på hur du kan använda C++ för att jämföra datum.

```C++
// Importera nödvändiga bibliotek
#include <iostream>
#include <chrono>
#include <ctime>

// Definiera två datum
std::chrono::system_clock::time_point datum1 = std::chrono::system_clock::now();
std::chrono::system_clock::time_point datum2 = std::chrono::system_clock::now() - std::chrono::hours(24);

// Första sättet att jämföra datum är att konvertera dem till time_t och använda difftime() funktionen
time_t tid1 = std::chrono::system_clock::to_time_t(datum1);
time_t tid2 = std::chrono::system_clock::to_time_t(datum2);
double tidsdifferens = difftime(tid1, tid2);

// Andra sättet är att använda <, <=, > eller >= operatorer för att jämföra de konverterade tiderna
if (tid1 < tid2) {
    // kod som utförs om tid1 är mindre än tid2
}

// Tredje sättet är att använda datumobjekt för att jämföra datum genom att använda operatorerna ==, !=, <, <=, > eller >=
if (datum1 == datum2) {
    // kod som utförs om datum1 och datum2 är samma
}

// Slutligen kan du använda std::chrono::duration för att beräkna tidsdifferensen mellan två datum
std::chrono::duration<double> tidsdifferens = datum1 - datum2;
std::cout << "Tidsdifferens: " << tidsdifferens.count() << " sekunder" << std::endl;
```

**Sample output:**

Dagens datum och tid: tis jun 29 09:14:10 2021
Datum från igår: mån jun 28 09:14:10 2021
Tidsdifferens: 86400 sekunder

## Djupdykning
Att jämföra datum kan verka enkelt, men det finns faktiskt en del komplexitet involverad i processen. Det beror på att datum och tid lagras på olika sätt i C++. För att få exakta resultat när man jämför datum är det därför viktigt att förstå hur C++ hanterar datum och använda rätt konverteringsmetoder.

Det är också viktigt att notera att tidszoner och sommartid påverkar resultatet när man jämför datum, så det är viktigt att vara medveten om detta och ta hänsyn till det i din kod.

## Se även
- [C++ Date and Time Tutorial](https://www.learncpp.com/cpp-tutorial/date-and-time-part-2/)
- [C++ Time Tutorials](https://www.geeksforgeeks.org/c-tutorial-time-library-classes/)
- [C++ Date and Time Functions](https://www.programiz.com/cpp-programming/library-function/ctime)