---
title:    "C: Konvertere en dato til en streng"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggen skal vi se på hvordan man kan konvertere en dato til en streng i C-programmering. Dette er en essensiell ferdighet for å kunne behandle og presentere datoer i et leselig format. Enten det er for å vise datoer til brukere eller å lagre datoer i en database, er det ofte nødvendig å konvertere datoer til strenger.

## Slik gjør du det

For å konvertere en dato til en streng i C, må vi først deklarere en dato-variabel ved hjelp av structen `tm`. Deretter kan vi bruke funksjonen `strftime` for å konvertere datoen til en streng. Her er et eksempel på hvordan dette kan gjøres:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Deklarerer en struct for dato og tid
    struct tm date;
    
    // Setter verdier for dato-variabelen
    date.tm_mday = 15; // Dagen
    date.tm_mon = 11; // Måneden (januar = 0, desember = 11)
    date.tm_year = 2021; // Året
   
    // Bruker strftime for å konvertere datoen til en streng
    char dato[20]; // Lager et array for å lagre strengen
    strftime(dato, 20, "%d/%m/%Y", &date); // Definerer formatet til strengen
    
    printf("Datoen er: %s\n", dato); // Printer ut strengen
    
    return 0;
}
```

Output: Datoen er: 15/12/2021

Som vi kan se i eksempelet, bruker vi `strftime`-funksjonen til å definere formatet til strengen vi ønsker å konvertere datoen til. Dette gjør det mulig å tilpasse strengen etter vårt behov. Det finnes en rekke forskjellige formateringsalternativer, og man kan også legge til spesielle symboler eller tekst for å gjøre strengen mer leselig. Du kan lære mer om disse alternativene i "Deep Dive"-delen nedenfor.

## Dykk ned i det

Det er viktig å forstå hvordan datoen er representert i C-programmering. En dato er vanligvis representert ved hjelp av en struct kalt `tm`. Denne structen inneholder medlemmer som representerer elementer som dato, måned, år, time og minutt. Vi kan bruke disse medlemmene til å sette eller få tilbake forskjellige deler av datoen.

Når det gjelder formatering av datoen til en streng, bruker `strftime`-funksjonen ulike formateringsalternativer som starter med et prosenttegn (%). Her er noen av de vanligste alternativene:

- `%d`: Dagen (to-sifret)
- `%m`: Måneden (to-sifret)
- `%Y`: Året (fire-sifret)
- `%H`: Timen (24-timers format)
- `%M`: Minutt (to-sifret)
- `%S`: Sekund (to-sifret)

Det finnes også mange andre alternativer, som for eksempel `%b` for å få måneden i tekstformat, eller `%a` for å få dagen i tekstformat. Det er også mulig å kombinere disse alternativene for å få et mer spesifikt og leselig format.

## Se også

- [C strftime() funksjonen](https://www.programiz.com/c-programming/library-function/strftime)
- [C-dato og tid Tutorial](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Hvordan representere dato og tid i C](https://www.guru99.com/c-date-time.html)