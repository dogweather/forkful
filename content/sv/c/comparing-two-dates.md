---
title:                "Jämförelse av två datum"
html_title:           "C: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vad & Varför?
 Att jämföra två datum är en vanlig uppgift för programmerare. Det handlar helt enkelt om att jämföra två specifika datum och se vilket som är tidigare eller senare. Detta är viktigt för att kunna sortera och filtrera data i rätt ordning.

# Hur man: 
Detta kan enkelt göras med hjälp av språket C. Nedan följer två enkla exempel på hur man kan jämföra två datum och få ut den tidigaste och senaste:

```
// Exempel 1: Jämför två specifika datum
#include <stdio.h>
#include <time.h>

int main()
{
    // Skapa två struct-variabler för att lagra datumen
    struct tm datum1 = {0};
    struct tm datum2 = {0};

    // Ange första datumet
    datum1.tm_year = 2021;
    datum1.tm_mon = 11; // Januari = 0, Feb = 1, o.s.v.
    datum1.tm_mday = 15;
    
    // Ange andra datumet
    datum2.tm_year = 2020;
    datum2.tm_mon = 5;
    datum2.tm_mday = 20;

    // Omvandla datumen till tid med hjälp av mktime-funktionen
    time_t tid1 = mktime(&datum1);
    time_t tid2 = mktime(&datum2);

    // Jämför tiderna och skriv ut resultatet
    if (tid1 < tid2)
    {
        printf("%d-%d-%d kommer före %d-%d-%d\n", datum1.tm_year, datum1.tm_mon, datum1.tm_mday, datum2.tm_year, datum2.tm_mon, datum2.tm_mday);
    }
    else if (tid1 > tid2)
    {
        printf("%d-%d-%d kommer efter %d-%d-%d\n", datum1.tm_year, datum1.tm_mon, datum1.tm_mday, datum2.tm_year, datum2.tm_mon, datum2.tm_mday);
    }
    else
    {
        printf("%d-%d-%d är samma som %d-%d-%d\n", datum1.tm_year, datum1.tm_mon, datum1.tm_mday, datum2.tm_year, datum2.tm_mon, datum2.tm_mday);
    }

    return 0;
}

```

```
// Exempel 2: Jämför aktuellt datum med ett visst datum
#include <stdio.h>
#include <time.h>

int main()
{
    // Hitta aktuellt datum
    time_t nu = time(NULL);
    struct tm *aktuellt_datum = localtime(&nu);

    // Ange ett datum att jämföra med
    struct tm datum = {0};
    datum.tm_year = 2020;
    datum.tm_mon = 8;
    datum.tm_mday = 1;

    // Omvandla datumen till tid med mktime-funktionen
    time_t tid_nu = mktime(aktuellt_datum);
    time_t tid_andra = mktime(&datum);

    // Jämför tiderna och skriv ut resultatet
    if (tid_nu < tid_andra)
    {
        printf("Idag är före %d-%d-%d\n", datum.tm_year, datum.tm_mon, datum.tm_mday);
    }
    else if (tid_nu > tid_andra)
    {
        printf("Idag är efter %d-%d-%d\n", datum.tm_year, datum.tm_mon, datum.tm_mday);
    }
    else
    {
        printf("Idag är %d-%d-%d\n", datum.tm_year, datum.tm_mon, datum.tm_mday);
    }

    return 0;
}
```

Output:
```
2020-5-20 kommer före 2021-11-15
Idag är efter 2020-8-1
```

# Djupdykning:
Det som händer bakom kulisserna när man jämför två datum är att datumen omvandlas till sekunder sedan 1970-01-01 (Unix epoch) och sedan jämförs dessa tider. Det finns också olika sätt att jämföra datum på, till exempel genom att jämföra år, månad och dag separat istället för att omvandla till tid.

En annan intressant detalj är att man behöver ta hänsyn till skottår och hur månader med olika antal dagar påverkar jämförelsen.

# Se även:
- [https://www.tutorialspoint.com/c_standard_library/time_h.htm](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [https://www.tutorialspoint.com/cprogramming/c_struct_tm.htm](https://www.tutorialspoint.com/cprogramming/c_struct_tm.htm)
- [https://www.codesdope.com/c-datetime/](https://www.codesdope.com/c-datetime/)