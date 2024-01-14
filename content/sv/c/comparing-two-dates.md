---
title:                "C: Jämföra två datum"
programming_language: "C"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Jämföra två datum kan vara en viktig del av programmering, särskilt när det kommer till att hantera tid och datum i ett program. Genom att jämföra två datum kan du kontrollera ordningen av händelser eller se hur mycket tid som har gått mellan två specifika datum. I denna blogginlägg kommer vi att titta på hur man jämför två datum i ett C-program.

## Så här gör du

För att jämföra två datum i C behöver du använda en funktion som kallas `difftime()` som finns i `time.h` biblioteket. Denna funktion tar in två `time_t` variabler som representerar de två datum som du vill jämföra. Låt oss titta på ett exempel:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Skapa ett datum för idag
    time_t d1 = time(NULL);
    
    // Skapa ett datum för igår
    time_t d2 = d1 - (24 * 60 * 60);
    
    // Beräkna skillnaden i sekunder mellan de två datum
    double diff = difftime(d1, d2);
    
    // Skriv ut skillnaden i timmar
    printf("Skillnad mellan dagarna: %.2f timmar\n", diff/3600);
    
    return 0;
}
```

I detta exempel har vi skapat två `time_t` variabler för dagens datum och gårdagens datum. Genom att använda funktionen `difftime()` har vi beräknat skillnaden i sekunder mellan de två datum. Slutligen skriver vi ut skillnaden i timmar för att få en mer lättförståelig utgång.

Output:

```
Skillnad mellan dagarna: 24.00 timmar
```

Det är viktigt att notera att `difftime()` returnerar ett flyttal, vilket gör det möjligt att beräkna mer exakta skillnader. För mer komplexa jämförelser kan du också använda `struct tm` från `time.h` för att representera datum och tider.

## Deep Dive

En intressant aspekt av att jämföra två datum är att ta hänsyn till skottårsdagar. När man jämför datum, är det viktigt att ta hänsyn till att vissa år har en extra dag på grund av skottdag. Detta kan påverka antalet dagar mellan två datum på ett oväntat sätt. 

En annan aspekt att ta hänsyn till är tidszoner. Om du jämför två datum som är i olika tidszoner kan du behöva konvertera dem till samma tidszon innan du utför jämförelsen för att få korrekta resultat. Detta kan göras genom att använda funktioner som `localtime()` och `gmtime()` för att konvertera `time_t` variabler till respektive lokala eller GMT-tider.

## Se även

- [C Programming Tutorial in Swedish](https://github.com/tclindner/C-Programmering/)
- [Officiell C-dokumentation på svenska](https://sv.cppreference.com)