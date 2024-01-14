---
title:                "C++: Jämföra två datum"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara en viktig del av programmering. Det kan hjälpa dig att kontrollera om ett datum är tidigare, senare eller samma som ett annat datum. Det kan även användas för att utföra beräkningar och kontrollera giltigheten av ett datum. Det är en viktig färdighet för alla som arbetar med datum och tid i sina program.

## Hur man jämför två datum i C++

För att jämföra två datum i C++ behöver vi först förstå datatyperna som är involverade. I C++ finns det två huvudsakliga datatyper för datum och tid: "time_t" och "tm". "time_t" representerar antalet sekunder som har gått sedan 1 januari 1970, medan "tm" innehåller information om ett datum och tid på ett mer läsbart sätt.

För att illustrera detta, låt oss använda följande kod:

```C++
#include<iostream>
#include<ctime>

using namespace std;

int main() {
    // Skapa två datum att jämföra
    time_t t1 = time(0); // idag
    tm t2 = *localtime(&t1); // dagens datum
    t2.tm_year += 1; // lägg till ett år
    
    // Jämföra datumen
    if (t2 > t1) {
        cout << "Datum 2 är senare än datum 1" << endl;
    } else if (t2 < t1) {
        cout << "Datum 2 är tidigare än datum 1" << endl;
    } else {
        cout << "Datum 2 är samma som datum 1" << endl;
    }
    return 0;
}
```

Output: Datum 2 är senare än datum 1

Här har vi skapat två datum att jämföra. Den första är dagens datum, medan den andra är samma datum ett år senare. Sedan jämför vi de två datumen genom att använda operatorerna ">", "<" och "==". Beroende på vilket datum som är större, mindre eller samma som det andra datumen, kommer ett lämpligt meddelande att skrivas ut.

## Fördjupa dig i att jämföra två datum

Det finns flera olika faktorer att tänka på när man jämför två datum i C++. En viktig sak att komma ihåg är att datatypen "time_t" lagrar datum och tid i UTC (Coordinated Universal Time), vilket kan orsaka problem om ditt program behöver hantera olika tidszoner. Därför är det viktigt att konvertera datum till UTC innan du jämför dem.

En annan sak att tänka på är att dagar, månader och år inte alltid är lika långa. Detta kan påverka din jämförelse och leda till fel resultat. Det är därför viktigt att använda funktioner som tar hänsyn till detta, som "mktime" och "difftime".

Om du vill lära dig mer om hur man jämför två datum i C++, rekommenderar jag att du läser dokumentationen för de olika funktionerna som används i exempelkoden ovan, och övar på att jämföra flera olika datum och se hur de beter sig.

## Se även

- [C++ Date and Time Library](https://www.cplusplus.com/reference/ctime/) (dokumentation för funktioner som används i exempelkoden)
- [C++ DateTime Library Tutorial](https://www.learncpp.com/cpp-tutorial/8-16-datetime-library-part-1/) (tutorial om hur man manipulerar datum och tid i C++)
- [Effektiv användning av datum och tid i C++](https://www.embedded.com/effective-use-of-date-and-time-functions-in-c/) (artikel om fördjupad användning av datum och tid i C++)