---
title:    "C: Konvertere en dato til en streng"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger kan det hende at du trenger å konvertere en dato til en tekststreng i C-programmering. Dette kan være nyttig når du for eksempel ønsker å vise en dato på en nettside eller i et annet brukergrensesnitt. I denne bloggposten vil vi gå gjennom hvordan du kan gjøre dette på en enkel måte.

## Hvordan
``` C
#include <stdio.h>
#include <string.h>
#include <time.h>

int main() {
    // Oppretter en struct for dagens dato
    struct tm *today;
    time_t now;
    time(&now);
    today = localtime(&now);

    // Konverterer datoen til en tekststreng
    char date_string[20];
    strftime(date_string, 20, "%d.%m.%Y", today);

    // Skriver ut datoen
    printf("Dagens dato er: %s\n", date_string);

    return 0;
}
```

Output:
```
Dagens dato er: 03.11.2021
```

I dette eksemplet bruker vi funksjonen "strftime" fra "time.h"-biblioteket til å konvertere datoen til en tekststreng. Vi spesifiserer også formatet vi ønsker for tekststrengen, i dette tilfellet "%d.%m.%Y" som betyr dag.måned.år.

En annen måte å gjøre dette på er å bruke funksjonen "sprintf" som lar oss skrive tekst direkte til en variabel, for eksempel:

```C
sprintf(date_string, "%d.%d.%d", today->tm_mday, today->tm_mon + 1, today->tm_year + 1900);
```

Det er viktig å merke seg at måned og år må justeres med henholdsvis +1 og +1900.

## Dypdykk
Det finnes også andre måter å konvertere datoer til tekst på, for eksempel ved å bruke "mktime" og "localtime" funksjonene til å lage en tidspunkt-datastruktur og deretter bruke "asctime" for å konvertere den til en tekststreng. Det er også mulig å bruke "snprintf" istedenfor "sprintf" for å unngå potensielle bufferoverløp.

En annen viktig ting å huske på når man konverterer datoer til tekst er formatering. Det er viktig å velge et passende format for tekststrengen basert på hvordan datoen vil bli brukt senere.

## Se også
- [https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)
- [https://www.tutorialspoint.com/c_standard_library/c_function_asctime.htm](https://www.tutorialspoint.com/c_standard_library/c_function_asctime.htm)
- [https://www.tutorialspoint.com/c_standard_library/c_function_time.htm](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)