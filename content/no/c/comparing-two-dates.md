---
title:    "C: Sammenligning av to datoer"
keywords: ["C"]
---

{{< edit_this_page >}}

# Hvorfor

Sammenligning av to datoer er en vanlig oppgave i mange programmeringsprosjekter. Det lar deg sjekke om en dato kommer før eller etter en annen, og kan også brukes til å beregne tidsforskjellen mellom to datoer. Med riktig kunnskap om hvordan man gjør dette i C-programmering, kan du forbedre funksjonaliteten og nøyaktigheten til programmene dine.

#Slik gjør du det

Det finnes ulike måter å sammenligne to datoer på i C-programmering. Det enkleste er å bruke standardbiblioteket <time.h>, som gir funksjoner for å håndtere datoer og klokkeslett.

Først må du konvertere de to datoene du ønsker å sammenligne til en type som kan sammenlignes, for eksempel en variabel av typen `time_t`. Dette kan gjøres ved hjelp av funksjonen `mktime()` som tar inn en `struct tm` som inneholder datoinformasjonen.

 ```C
#include <stdio.h>
#include <time.h>

int main()
{
    // Opprett to datostrukturer
    struct tm dato1, dato2;
    
    // Sett datoinformasjonen for hver struktur
    dato1.tm_year = 2021 - 1900; // Årstall minus 1900
    dato1.tm_mon = 5; // Måneden (0-11)
    dato1.tm_mday = 1; // Datoen (1-31)
    
    dato2.tm_year = 2021 - 1900;
    dato2.tm_mon = 6;
    dato2.tm_mday = 10;
    
    // Konverter til time_t
    time_t t1 = mktime(&dato1);
    time_t t2 = mktime(&dato2);
    
    if(t1 < t2)
    {
        printf("Dato 1 kommer før dato 2");
    }
    else if(t1 > t2)
    {
        printf("Dato 1 kommer etter dato 2");
    }
    else
    {
        printf("Dato 1 er lik dato 2");
    }
    
    return 0;
}
```

For å beregne tidsforskjellen mellom to datoer, kan du bruke funksjonen `difftime()` som tar inn to `time_t` variabler og returnerer differansen i sekunder.

# Dykke dypere

Hvis du ønsker å sammenligne flere aspekter av datoene, som for eksempel år, måned eller dag, kan du hente ut disse verdiene fra datoenes `struct tm` ved å bruke funksjoner som `tm_year`, `tm_mon` og `tm_mday`. Du kan også bruke funksjonen `strftime()` for å formatere datoer i ulike formater.

Det er også verdt å merke seg at `time_t` og `struct tm` variabler bruker lokal tid. Hvis du ønsker å sammenligne datoer i en annen tidssone, må du først konvertere til UTC.

# Se også

- <https://www.tutorialspoint.com/c_standard_library/time_h.htm>
- <https://www.programiz.com/c-programming/library-function/time/mktime>
- <https://www.programiz.com/c-programming/library-function/time/difftime>
- <https://www.programiz.com/c-programming/library-function/time/strftime>