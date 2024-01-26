---
title:                "Skriving av en tekstfil"
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive en tekstfil i C++ betyr å lagre data som tekst på en fil i filsystemet. Programmerere gjør det for å lagre resultater, konfigurasjonsdata eller til og med logge hva programmet gjør.

## Hvordan:

For å skrive en tekstfil i C++, bruker vi I/O-biblioteket '<fstream>'. Her er en enkel kodebit som skaper og skriver til en tekstfil:

```C++
#include <iostream>
#include <fstream>

int main() {
    std::ofstream file("eksempel.txt");

    if(file.is_open()) {
        file << "Hei, dette er en tekstfil!\n";
        file << "Her er noe mer tekst.\n";
        file.close();
    } else {
        std::cerr << "Kunne ikke åpne filen." << std::endl;
    }

    return 0;
}
```
Når du kjører koden over, vil den lage 'eksempel.txt' og den inneholder:

```
Hei, dette er en tekstfil!
Her er noe mer tekst.
```

## Dypdykk

Historisk sett har filskriving vært grunnleggende for langtidsdataoppbevaring. Alternativt kan programmerere bruke databaser for strukturert data, eller nyere løsninger som skytjenester. I C++ sikrer klassen 'ofstream' effektiv skriving til filer, og dette har vært standarden siden introduksjonen av STL (Standard Template Library). Husk å håndtere mulige feil som f.eks. manglende skrivetilganger eller fulle lagringsenheter.

## Se Også

- C++ Referanse for `<fstream>`: http://www.cplusplus.com/reference/fstream/
- C++ File I/O i CPP Reference: https://en.cppreference.com/w/cpp/io
- C++ Tutorials om filhåndtering: https://www.learncpp.com/cpp-tutorial/186-basic-file-io/
