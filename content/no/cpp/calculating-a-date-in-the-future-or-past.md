---
title:                "C++: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen ønske å beregne en dato i fremtiden eller fortiden? Vel, det er flere grunner til det. Kanskje du trenger å planlegge en viktig hendelse, som en bursdagsfest eller et bryllup. Eller kanskje du jobber med et prosjekt som har en bestemt tidsramme, og du trenger å vite når en bestemt dato faller på. Uansett grunn, å kunne beregne datoer er en viktig ferdighet som kan komme til nytte i mange ulike situasjoner.

# Hvordan

Å beregne en dato i fortiden eller fremtiden er ganske enkelt hvis du bruker riktig kode. Her er et eksempel på hvordan du kan gjøre det i C++:
```C++
#include <iostream> 
#include <ctime> 

int main() 
{ 
    // Sett opp dagens dato
    time_t now = time(0);
    tm *ltm = localtime(&now);

    // Beregn en dato 30 dager frem i tid
    ltm->tm_mday += 30; 

    // Bruk mktime for å konvertere datoen tilbake til time_t format 
    now = mktime(ltm); 

    // Skriv ut den beregnede datoen
    std::cout << "30 dager fra nå vil det være " << ctime(&now); 

    return 0; 
}
```

Output:
```
30 dager fra nå vil det være <dato i fremtiden>
```

# Dypdykk

Å beregne datoer kan virke enkelt, men det er faktisk noen nøkkelaspekter du bør være oppmerksom på for å sikre nøyaktige resultater. For det første må du passe på at du bruker riktig tidsformat for datoen du ønsker å beregne. Det kan være forskjellige tidsformater og kalendere som brukes i ulike land, så det er viktig å dobbeltsjekke dette for å unngå feil.

Videre kan det være nyttig å bruke innebygde funksjoner i C++ biblioteket som ```mktime``` og ```localtime```, som gjør det enklere å konvertere datoen til riktig format.

Det er også viktig å huske på at beregning av datoer kan bli mer komplekst når man tar hensyn til årskifter, skuddår og ulike tidssoner. Det kan derfor være lurt å dobbeltsjekke beregningene dine, spesielt når du jobber med historiske datoer.

# Se også

Her er noen nyttige ressurser hvis du ønsker å utforske mer om beregning av datoer i C++:

- [C++ tid og dato bibliotek](https://www.cplusplus.com/reference/ctime/)
- [Stack Overflow spørsmål om å beregne datoer i C++](https://stackoverflow.com/questions/7932506/how-to-calculate-time-difference-in-c)
- [C++ referanser for datering og tidsformatering](https://en.cppreference.com/w/c/chrono)

Lykke til med å beregne datoer i C++!