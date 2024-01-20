---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att bestämma om ett datum förekommer före, efter eller sammanfaller med ett annat datum. Programmerare gör detta för att hantera och beräkna tid och datum i deras program och applikationer.

## Så gör du:
Här är ett enkelt sätt att jämföra två datum med C++:

```C++
#include <iostream>
#include <ctime>

int main () {
  std::time_t t = std::time(0);  // get time now
  std::tm* now = std::localtime(&t);

  std::tm a = {0,0,0,5,10,121}; //Set a to Nov 5, 2021
  std::tm b = {0,0,0,now->tm_mday,now->tm_mon,now->tm_year}; //Set b to today's date
  std::time_t x = std::mktime(&a);
  std::time_t y = std::mktime(&b);
  
  if (x != (std::time_t)(-1) && y != (std::time_t)(-1)) {
    double difference = std::difftime(y, x) / (60 * 60 * 24);
    std::cout << "Difference in days : " << difference << "\n";
  }
  return 0;
}
```

I ovanstående kod beräknar vi skillnaden mellan dagens datum och ett specifikt datum i dagar. Om du kör denna kod, kommer utmatningen vara antalet dagar mellan de två datumen.

## Djup Dykning:
(1) I den tidiga tiden av programmering fanns det inte någon standardiserad funktion för att jämföra datum, vilket ledde till en rad olika metoder och kodstycken för att utföra denna enkla uppgift. Men med C++ standardbiblioteket, kan vi nu jämföra datum lätt och effektivt.

(2) Alternativa sätt att jämföra datum inkluderar att omvandla datum till Julian datum, eller att använda tredjepartsbibliotek som "boost" eller "date.h". Men för de flesta användningsområden är C++ standardbiblioteket mer än tillräckligt.

(3) I termer av implementationsdetaljer, beräknar `difftime()` funktionen skillnaden i sekunder mellan två `time_t` värden och konverterar sedan detta till dagar.

## Se Även:
1. [`std::time` definition på cppreference.com](https://en.cppreference.com/w/cpp/chrono/c/time)

2. [`std::tm` strukturen på cppreference.com](https://en.cppreference.com/w/cpp/chrono/c/tm)

3. [Deep Dive into C++ Date and Time på stackoverflow.com](https://stackoverflow.com/questions/997946/how-to-get-current-time-and-date-in-c)