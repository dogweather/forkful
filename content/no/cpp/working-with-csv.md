---
title:                "C++: Å arbeide med csv"
simple_title:         "Å arbeide med csv"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har jobbet med større datasett eller informasjonsfiler, har du sannsynligvis støtt på filtypen CSV (komma-separert verdi). Dette er en vanlig måte å lagre tabulære data på, og det er et format som er enkelt å arbeide med for både mennesker og datamaskiner. I denne bloggposten skal vi se nærmere på hvordan du kan bruke C++ til å jobbe med CSV-filer.

## Hvordan

Først må du inkludere biblioteket <fstream> for å kunne lese og skrive til filer. Deretter kan du bruke funksjonen "getline" for å lese innholdet av en CSV-fil linje for linje. La oss ta et eksempel:

```C++
#include <fstream>
#include <iostream>

int main() {
    // Åpne CSV-filen for lesing
    std::ifstream csvFile("datasett.csv");

    // Sjekk at filen er åpen
    if (!csvFile.is_open()) {
        std::cout << "Kunne ikke åpne filen for lesing." << std::endl;
        return 1;
    }

    // Les innholdet av filen linje for linje og skriv det ut til konsollen
    std::string line;
    while (getline(csvFile, line)) {
        std::cout << line << std::endl;
    }

    // Husk å lukke filen når du er ferdig med å lese
    csvFile.close();

    return 0;
}
```

I dette eksempelet bruker vi ifstream-objektet til å lese fra filen "datasett.csv". Vi sjekker også om filen faktisk er åpen før vi begynner å lese den. Deretter bruker vi getline-funksjonen til å lese innholdet av filen linje for linje. Til slutt lukker vi filen når vi er ferdig med å lese.

Output vil se noe lignende dette ut:

```
Navn; Alder; Bosted
Per; 35; Oslo
Line; 27; Bergen
Ola; 42; Trondheim
```

## Dypdykk

Nå som vi har sett på hvordan du kan lese CSV-filer i C++, kan det være nyttig å dykke litt dypere ned i hvordan du kan jobbe med disse filene. CSV-filer kan ofte inneholde tusenvis av linjer med data, så det er viktig å være effektiv i hvordan du leser og behandler disse filene. Her er noen tips til hvordan du kan forbedre ytelsen når du jobber med CSV-filer i C++:

- Unngå å bruke tekstformattering når du skriver til filen. Dette kan ta mye lengre tid enn å skrive rå tekst.
- Bruk gjerne en parser eller et ferdig bibliotek for å håndtere CSV-filer, istedenfor å implementere alt selv.
- Hvis du skal lese en stor CSV-fil, kan det være lurt å bruke multitråding for å forbedre ytelsen.

Husk også at CSV-filer kan ha forskjellige formater og separeringskarakterer, så det kan være lurt å sjekke hvordan filen er strukturert før du begynner å jobbe med den.

## Se også

- [C++ for nybegynnere](https://www.cplusplus.com/doc/tutorial/)
- [Mer om å jobbe med filer i C++](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Enkelt å bruke CSV-parser for C++](https://github.com/ben-strasser/fast-cpp-csv-parser)