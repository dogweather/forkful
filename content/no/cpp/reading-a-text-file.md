---
title:    "C++: Lesing av en tekstfil"
keywords: ["C++"]
---

{{< edit_this_page >}}

# Hvorfor lese en tekstfil i C++?

Lesing av en tekstfil er en viktig oppgave i programmering, spesielt i C++. Det kan hjelpe deg med å håndtere store mengder data og gjøre det enklere å manipulere og bruke informasjonen den inneholder.

# Hvordan lese en tekstfil i C++?

For å lese en tekstfil i C++, må vi først åpne filen ved hjelp av fstream biblioteket. Deretter kan vi bruke en while-løkke for å lese hver linje i filen og lagre dem i en string-variabel. Dette kan se ut som følgende eksempel:

```C++
#include <fstream> // Inkluderer fstream biblioteket
#include <string> // Inkluderer string biblioteket

int main() {
  std::ifstream file("tekstfil.txt"); // Åpner filen

  std::string line; // Oppretter en string-variabel for å lagre hver linje i filen

  // Bruker en while-løkke for å lese hver linje i filen og skrive den ut
  while (std::getline(file, line)) {
    std::cout << line << '\n';
  }

  file.close(); // Lukker filen når vi er ferdig med å lese den

  return 0;
}
```

Eksempel output: 
```
Dette er en linje i en tekstfil
Dette er en annen linje
Dette er en tredje linje
```

# Dykk dypere

I tillegg til å lese linje for linje, kan vi også bruke en annen metode for å lese en tekstfil i C++. Vi kan bruke en stringstream for å dele opp en linje i flere deler basert på et skilletegn, for eksempel komma eller tabulator. Dette kan være nyttig hvis filen vår har en bestemt format som vi ønsker å følge. Et eksempel på dette kan se slik ut:

```C++
#include <fstream> // Inkluderer fstream biblioteket
#include <string> // Inkluderer string biblioteket
#include <sstream> // Inkluderer stringstream biblioteket

int main() {
  std::ifstream file("datafil.txt"); // Åpner filen

  std::string line; // Oppretter en string-variabel for å lagre hver linje i filen

  // Bruker en while-løkke for å lese hver linje i filen
  while (std::getline(file, line)) {
    std::stringstream linje(line); // Oppretter en stringstream med linjen vi leser
    std::string data; // Oppretter en string-variabel for å lagre hver del av linjen

    // Bruker en nested while-løkke for å lese hver del av linjen basert på skilletegnet komma
    while (getline(linje, data, ',')) {
      std::cout << data << '\n';
    }
  }

  file.close(); // Lukker filen når vi er ferdig med å lese den

  return 0;
}
```

Eksempel output: 
```
Dette er første data
Dette er andre data
Dette er tredje data
```

# Se også

- [Hvordan skrive til en tekstfil i C++](https://www.programiz.com/cpp-programming/files-input-output)
- [Lesing og skriving av CSV-filer i C++](https://www.geeksforgeeks.org/csv-file-management-using-c/)
- [C++ stringstream](https://www.cplusplus.com/reference/sstream/stringstream/)