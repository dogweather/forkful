---
title:    "C++: Konvertere en dato til en streng"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

##Hvorfor
Konvertering av datoer til strenger er en vanlig oppgave i programmering, spesielt når man skal vise datoer til brukeren i et brukergrensesnitt. Det kan også være nyttig for å lagre datoer i en database eller i en fil.

##Hvordan
For å konvertere en dato til en streng i C++, kan man bruke funksjonen `strftime()`. Denne funksjonen tar inn tre parametere: en streng som beskriver formatet på datoen, en `struct tm`-variabel som inneholder datoens informasjon, og til slutt en streng som vil få datoen til å bli lagret i. La oss se på et eksempel:

```C++
#include <iostream> 
#include <ctime> 

int main() 
{ 
    // Opprett dato-structen 
    struct tm dato = {0}; 

    // Sett datoens informasjon 
    dato.tm_year = 2020 - 1900; // Året starter på 1900, så her bruker vi 2020 
    dato.tm_mon = 7 - 1; // Måneden starter på 0, så her bruker vi 7 for å representere juli 
    dato.tm_mday = 1; // Dagen starter på 1 

    // Opprett en buffer for å lagre datoen 
    char buffer[11]; 

    // Bruk strftime() til å konvertere datoen til en streng 
    strftime(buffer, 11, "%d-%m-%Y", &dato); 

    // Skriv ut datoen 
    std::cout << buffer; 

    return 0; 
}
```

Som resultat vil vi få "01-07-2020". Her har vi brukt formatet dd-mm-yyyy, men man kan velge et format som passer best for ens egne behov.

Det er også verdt å merke seg at `struct tm`-variabelen er fra standardbiblioteket `<ctime>`. Dette biblioteket er også ansvarlig for å håndtere tid og dato-operasjoner.

##Dypdykk
Når vi bruker `strftime()`-funksjonen, kan vi også bruke ulike formater for å få datoen til å se annerledes ut. For eksempel, hvis vi vil få datoen til å vise navnet på måneden i stedet for tallet, kan vi bruke %B-formatet i stedet for %m-formatet.

Her er en liste over noen av de vanligste formatene som kan brukes i `strftime()`:
- %d: Dagen i måneden (01-31)
- %m: Måneden i året (01-12)
- %Y: Året (f.eks. 2020)
- %H: Timen (00-23)
- %M: Minuttene (00-59)
- %S: Sekundene (00-59)
- %A: Navnet på ukedagen (f.eks. Monday)
- %B: Navnet på måneden (f.eks. July)

For en komplett liste over alle formatene, kan man se på dokumentasjonen til `strftime()`-funksjonen.

##Se også
- [Dokumentasjon for <ctime> biblioteket (engelsk)](https://en.cppreference.com/w/cpp/header/ctime)
- [Informasjon om `strftime()`-funksjonen (engelsk)](https://www.cplusplus.com/reference/ctime/strftime/)