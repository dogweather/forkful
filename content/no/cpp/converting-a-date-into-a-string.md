---
title:    "C++: Å konvertere en dato til en streng"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en dato til en streng er en vanlig utfordring i C++ programmering. Dette kan være nyttig når du for eksempel jobber med brukergrensesnitt og trenger å vise en dato på en spesifikk måte.

## Hvordan

Det er flere måter å konvertere en dato til en streng i C++. En av de enkleste måtene er å bruke funksjonen `strftime` fra `ctime` biblioteket. Her er et eksempel på hvordan du kan bruke denne funksjonen:

````C++
#include <iostream>
#include <ctime>

int main()
{
    // Opprett en struct for dagens dato
    time_t now = time(0);
    tm *current_date = localtime(&now);

    // Konverter datoen til streng
    char date_string[80];
    strftime(date_string, sizeof(date_string), "Dagens dato er: %d-%m-%Y", current_date);

    // Skriv ut datoen
    std::cout << date_string << std::endl;

    return 0;
}
````
Dette eksempelet vil skrive ut dagens dato i formatet "DD-MM-YYYY", for eksempel "Dagens dato er: 20-08-2021". Du kan også endre formatet i henhold til dine ønsker ved å endre parameterne til `strftime` funksjonen.

## Deep Dive

`strftime` funksjonen bruker en såkalt "formatstreng" for å bestemme hvordan datoen skal konverteres til en streng. Denne strengen består av spesielle tegn som representerer forskjellige deler av datoen. For eksempel representerer "%d" dag i måneden og "%m" måned i året. Det finnes mange forskjellige tegn som kan brukes i en formatstreng, og du kan finne en fullstendig liste ved å søke på nettet.

Det er også viktig å merke seg at denne metoden for konvertering fungerer bare for standard C++ datostrukturer. Hvis du for eksempel bruker et tredjeparts dato bibliotek, må du bruke dets egne funksjoner for konvertering til streng.

## Se Også

- [strftime reference](https://www.cplusplus.com/reference/ctime/strftime/)
- [Dato og klokkeslett i C++](https://www.cplusplus.com/reference/ctime/) (på engelsk)
- [C++ bibliotek og dato og tid](https://www.learncpp.com/cpp-tutorial/date-and-time/) (på engelsk)