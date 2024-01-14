---
title:                "C: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor ville noen ønske å konvertere en dato til en streng i programmering? Vel, det er flere grunner til dette. En vanlig situasjon er når du trenger å vise en dato i en leselig form for brukeren, for eksempel på et nettside eller i en applikasjon. Å konvertere datoen til en streng gjør det enklere å formatere og vise til brukeren.

## Hvordan

```C 
#include <stdio.h>
#include <time.h>

int main()
{
    //Oppretter en tidsstruktur
    struct tm *t;
    time_t now;
    
    //Henter nåværende tid
    time(&now);
    
    //Bruker localtime-funksjonen for å konvertere datoen til en tidsstruktur
    t = localtime(&now);
    
    //Bruker strftime-funksjonen for å konvertere tidsstrukturen til en streng
    char date_string[20];
    strftime(date_string, 20, "%d.%m.%Y", t);
    
    //Skriver ut datoen som en streng i det valgte formatet (dd.mm.åååå)
    printf("Dagens dato: %s", date_string);
    
    return 0;
}
```

### Output

```
Dagens dato: 14.02.2021
```

## Dykk ned

Å konvertere en dato til en streng kan virke som en enkel oppgave, men det finnes ulike måter å gjøre det på og flere aspekter å ta hensyn til. For eksempel kan formatet på datoen være forskjellig i ulike land og kulturer. Derfor er det viktig å velge riktig funksjon og passende format for den ønskede utgangen.

Det er også verdt å nevne at i noen programmeringsspråk, som Python, kan datoen konverteres til en streng uten behov for ekstra funksjoner. Men i C er det nødvendig å bruke spesialiserte funksjoner som strftime for å få ønsket utgang.

## Se også

- [strftime funksjonen i C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Hvordan formatere dato og tid i C](https://www.geeksforgeeks.org/date-time-programming-in-c-with-examples/?ref=lbp)
- [C programmering for nybegynnere på norsk](https://www.ntnu.no/wiki/display/prog/
C+programmering+for+nybegynnere+-+Leksjon+1)