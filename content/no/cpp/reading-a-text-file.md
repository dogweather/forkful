---
title:                "Lesing av tekstfil"
html_title:           "C++: Lesing av tekstfil"
simple_title:         "Lesing av tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle man engasjere seg i å lese en tekstfil? Vel, det er mange situasjoner hvor man kan dra nytte av å kunne lese en tekstfil med hjelp av C++ kode. Det kan være å hente ut data fra en ekstern fil, eller å analysere teksten i en fil for å trekke ut viktig informasjon.

## Hvordan

La oss se på et enkelt eksempel på hvordan man kan lese en tekstfil ved hjelp av C++. Først må vi inkludere "iostream" biblioteket for å håndtere IO-operasjoner. Deretter må vi åpne filen som vi ønsker å lese ved å bruke "ifstream" funksjonen:

```C++
#include <iostream>
#include <fstream>

int main() {

    std::ifstream file("minTekstfil.txt");
    // HER KAN VI UTFØRE OPERASJONER PÅ FILINNHOLD

    file.close(); // lukker filen etterpå

    return 0;
}
```

Nå som vi har åpnet filen, har vi tilgang til å lese den. La oss si at teksten i filen er "Hei verden!". Vi kan bruke "getline" funksjonen for å lese en linje av teksten fra filen og lagre den i en variabel:

```C++
#include <iostream>
#include <fstream>

int main() {

    std::ifstream file("minTekstfil.txt");

    std::string line;
    getline(file, line); // leser første linje av teksten og lagrer i variabelen "line"

    std::cout << line << std::endl; // skriver ut teksten i konsollen

    file.close();

    return 0;
}
```

Dette vil gi følgende utskrift:

```
Hei verden!
```

Vi kan også bruke en "while" loop for å lese gjennom hele filen linje for linje:

```C++
#include <iostream>
#include <fstream>

int main() {

    std::ifstream file("minTekstfil.txt");

    std::string line;
    while (getline(file, line)) { // så lenge det er flere linjer å lese
        std::cout << line << std::endl; // skriv ut linjen i konsollen
    }

    file.close();

    return 0;
}
```

Dette vil gi følgende utskrift:

```
Hei verden!
Dette er et annet linje.
Og enda en linje.
```

Vi kan også bruke ">>" operator for å lese enkle verdier fra filen, for eksempel tall eller bokstaver:

```C++
#include <iostream>
#include <fstream>

int main() {

    std::ifstream file("minTekstfil.txt");

    int tall;
    file >> tall; // leser tallet fra filen og lagrer i variabelen "tall"

    file.close();

    return 0;
}
```

## Dypdykk

Nå som vi har sett på noen grunnleggende eksempler på hvordan man kan lese en tekstfil, la oss se på noen dypere informasjon og muligheter.

C++ har flere måter å lese og skrive til tekstfiler på, inkludert "fstream", "ifstream" og "ofstream". "fstream" er den generelle klassen for å håndtere både lesing og skriving av tekstfiler, og er nyttig for å endre eller oppdatere eksisterende filer. "ifstream" er spesialisert for lesing av tekstfiler, mens "ofstream" er spesialisert for skriving til tekstfiler.

Det er også viktig å håndtere eventuelle feil som kan oppstå under lesing av en fil. Dette kan gjøres ved hjelp av "file.eof()" funksjonen, som sjekker om man har kommet til slutten av filen. Det er også viktig å sjekke om filen ble åpnet riktig før man begynner å lese fra den.

## Se også

- [C++ Filbehandling - w3schools](https://www.w3schools.com/cpp/cpp_files.asp)
- [C++ Filbehandling - Programiz](https://www.programiz.com/cpp-programming/file-handling)