---
title:                "Konvertering av dato til tekststreng"
html_title:           "C: Konvertering av dato til tekststreng"
simple_title:         "Konvertering av dato til tekststreng"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Å konvertere en dato til en streng er en vanlig oppgave for programmerere. Dette betyr rett og slett å ta en dato som er lagret i et datalager og gjøre om den til en tekststreng som kan leses og forstås av mennesker. Dette gjøres for å gjøre informasjon mer brukervennlig og lettere å tolke for sluttbrukere. 

# Hvordan:

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Definerer en dato
    struct tm dato = {
        .tm_year = 121, // Årstall siden 1900
        .tm_mon = 0,    // Måned (januar - 0, desember - 11)
        .tm_mday = 1    // Dag i måned
    };

    char datoStreng[50];

    // Konverterer datoen til en streng
    strftime(datoStreng, 50, "%A %d. %B %Y", &dato);

    // Skriver ut den konverterte strengen
    printf("Dato: %s\n", datoStreng);
    
    return 0;
}
```
Output: Dato: fredag 01. januar 2021

## Deep Dive

I de tidlige dagene av programmering var det vanlig å bruke numeriske verdier for å representere datoer. Dette var imidlertid ikke veldig brukervennlig og kunne føre til forvirring. Med utviklingen av mer avanserte datalagre og programmeringsspråk, har det blitt vanlig å konvertere datoer til tekststrenger for å gjøre informasjonen mer forståelig for brukere. I tillegg har bedre språkstøtte og biblioteker gjort denne prosessen enklere og mer effektiv.

Det finnes flere måter å konvertere en dato til en streng på, avhengig av programmeringsspråket du bruker. Noen programmeringsspråk har også dedikerte funksjoner for denne oppgaven, som gjør det enklere å implementere.

I eksempelet over viser vi en vanlig måte å konvertere en dato til en streng i C ved hjelp av funksjonen ```strftime```. Dette er en del av standardbiblioteket for C og gir deg muligheten til å formatere datoen som du ønsker. Det finnes også flere parameteralternativer som kan brukes for å skreddersy strengen etter dine behov, som for eksempel å legge til klokkeslett eller tidsone.

## Se også

Har du lyst til å lære mer om hvordan du bruker datofunksjoner i C? Her er noen ressurser du kan sjekke ut:

- [Offisiell C dokumentasjon for ```strftime```](https://www.cplusplus.com/reference/ctime/strftime/)
- [Eksempler på bruk av datofunksjoner i C](https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm)
- [Enkel guide for å konvertere datoer til strenger i C](https://www.guru99.com/date-string-conversion-in-c.html)