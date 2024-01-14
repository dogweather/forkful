---
title:                "C++: Leser en tekstfil"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Lesing og håndtering av tekstfiler er en viktig del av programmering, spesielt når man arbeider med større mengder data. Det å kunne lese og behandle en tekstfil kan være nyttig for å importere og eksportere data, samt å analysere store datamengder.

# Hvordan

For å lese en tekstfil i C++, må vi først opprette en instans av "ifstream" (input file stream) klassen. Deretter må vi åpne tekstfilen ved å bruke funksjonen "open()", og angi filnavnet som et argument. Her er et eksempel på hvordan koden kan se ut:

```C++
#include <iostream>
#include <fstream>

int main()
{
    // Oppretter en instans av ifstream klassen
    std::ifstream tekstfil;

    // Åpner tekstfilen "data.txt"
    tekstfil.open("data.txt");

    // Sjekker om filen ble åpnet riktig
    if (!tekstfil)
    {
        std::cout << "Kunne ikke åpne filen." << std::endl;
        return 1;
    }

    // Leser og skriver ut data fra filen linje for linje
    std::string linje;
    while (getline(tekstfil, linje))
    {
        std::cout << linje << std::endl;
    }

    // Lukker filen
    tekstfil.close();

    return 0;
}
```

Denne koden vil åpne tekstfilen "data.txt" og skrive ut innholdet linje for linje, frem til den når slutten av filen. Det er også mulig å bruke funksjonen "getline()" for å lese innholdet i filen som en streng og behandle det på andre måter.

# Dypdykk

Det finnes også andre måter å lese og håndtere tekstfiler i C++. For eksempel kan man bruke "fstream" klassen, som kombinerer både input og output funksjonalitet. Man kan også bruke funksjoner som "get()" og "ignore()" for å lese filen tegn for tegn.

Videre kan det være nyttig å lære om hvordan man håndterer feil og unntak når man arbeider med tekstfiler. Dette gjør at programmet kan håndtere uforutsette feil, som for eksempel en fil som ikke finnes eller ikke kan leses.

# Se også

- [Lesing og skriving av filer i C++](https://www.cplusplus.com/doc/tutorial/files/)
- [Håndtering av tekstfiler i C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Feilhåndtering i C++](https://www.geeksforgeeks.org/exception-handling-c/)