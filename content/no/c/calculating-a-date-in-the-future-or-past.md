---
title:    "C: Beregning av en dato i fremtiden eller fortiden"
keywords: ["C"]
---

{{< edit_this_page >}}

## Hvorfor

Det kan være flere grunner til å ønske å beregne en dato i fremtiden eller fortiden. Kanskje du skal planlegge en spesiell begivenhet, eller kanskje du ønsker å se hvordan en bestemt dato vil falle på en bestemt ukedag. Uansett årsak, kan det å kunne beregne en dato være nyttig for å organisere tidsplaner og planlegge fremover.

## Hvordan

Det er flere måter å beregne en dato i fremtiden eller fortiden på ved hjelp av et C-program. Her er et eksempel på hvordan du kan gjøre det:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Angi startdato i dagens dato
    time_t startdato = time(NULL);

    // Beregn antall sekunder i 30 dager
    long dager = 30 * 24 * 60 * 60;

    // Legg til antall sekunder i startdato
    startdato += dager;

    // Konverter tid til kalenderformat
    struct tm *dato = localtime(&startdato);

    // Skriv ut beregnet dato
    printf("Datoen om 30 dager vil være: %d-%d-%d\n",
           dato->tm_mday, dato->tm_mon + 1, dato->tm_year + 1900);

    return 0;
}
```

Utdata:

```
Datoen om 30 dager vil være: 14-3-2021
```

I dette eksempelet bruker vi funksjonen `time()` for å få dagens dato og tid, og deretter legger vi til et gitt antall sekunder for å beregne en fremtidig dato. Du kan også gjøre det samme for å beregne en dato i fortiden ved å trekke fra et antall sekunder fra startdatoen.

## Dypdykk

Hvis du vil lære mer om å beregne datoer i fremtiden eller fortiden, bør du studere C's `time` bibliotek grundig. Det finnes flere funksjoner som kan være nyttige i slike beregninger, som for eksempel `mktime()` og `strftime()`. Det er også viktig å forstå datotyper og formater i C for å kunne behandle datoer korrekt.

## Se også

- Dokumentasjon for `time` biblioteket i C: https://www.tutorialspoint.com/c_standard_library/time_h.htm
- Eksempler på å manipulere datoer i C: https://www.programiz.com/c-programming/library-function/time
- Artikkel om datohåndtering i C: https://www.geeksforgeeks.org/date-manipulation-in-c-c/