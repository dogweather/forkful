---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:31.377450-07:00
description: "Commandoregelargumenten stellen gebruikers in staat het gedrag van een\
  \ programma te be\xEFnvloeden zonder de code te wijzigen. Programma's gebruiken\
  \ ze om\u2026"
lastmod: '2024-03-11T00:14:24.966327-06:00'
model: gpt-4-0125-preview
summary: "Commandoregelargumenten stellen gebruikers in staat het gedrag van een programma\
  \ te be\xEFnvloeden zonder de code te wijzigen. Programma's gebruiken ze om\u2026"
title: Commandoregelargumenten lezen
---

{{< edit_this_page >}}

## Wat & Waarom?
Commandoregelargumenten stellen gebruikers in staat het gedrag van een programma te beïnvloeden zonder de code te wijzigen. Programma's gebruiken ze om invoerparameters, bestandspaden of bedieningsmodi te krijgen, wat tijd bespaart en flexibiliteit biedt.

## Hoe:
In C++ worden commandoregelargumenten ontvangen in `main()` als een array van karakterwijzers. Hier is hoe je ze ophaalt:

```C++
#include <iostream>
int main(int argc, char* argv[]) {
    std::cout << "Je hebt " << argc << " argumenten ingevoerd:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << argv[i] << "\n";
    }
    return 0;
}
```

Voorbeelduitvoer: (Aangenomen dat uitgevoerd als `./myProgram foo bar`)

```plaintext
Je hebt 3 argumenten ingevoerd:
./myProgram
foo
bar
```

## Diepere Duik
Lang geleden was de commandoregel de enige manier om met programma's te interageren. De GUI's van vandaag zijn geweldig, maar de commandoregel blijft bestaan, vooral in server- of ontwikkelomgevingen. Het biedt snelle, scriptbare controle.

Alternatieven voor ingebouwde `argv` en `argc` omvatten bibliotheken zoals `Boost.Program_options` voor uitgebreidere parsing. Er is ook de `getopt()` functie in Unix-achtige systemen voor meer traditionele liefhebbers van commandoregels.

Het implementeren van argumentenanalyse vanaf nul stelt je in staat deze aan te passen, maar let op veiligheidsrisico's. Vertrouw gebruikersinvoer niet blindelings—valideer en zuiver altijd.

## Zie Ook
- C++ documentatie over de `main()` functie: https://en.cppreference.com/w/cpp/language/main_function
- Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- GNU `getopt()` tutorial: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
