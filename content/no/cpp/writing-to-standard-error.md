---
title:                "Skriving til standardfeil"
html_title:           "C++: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?:
Når vi programmerer i C++, er det ofte viktig å skrive ut informasjon til konsollen, for eksempel inputs og outputs. Noen ganger ønsker vi imidlertid å skrive ut viktig informasjon, som feilmeldinger, til en annen strøm enn standard output. Dette er når vi bruker standard error. Ved å skrive til standard error, sikrer vi at viktig informasjon blir tydelig synlig og ikke blandet opp med vanlige utskrifter.

Hvordan:
For å skrive til standard error i C++, bruker vi uttrykket "std::cerr". La oss se et eksempel:
```C++
#include <iostream>
 
int main()
{
    int age = 35;
    std::cerr << "Alder: " << age << std::endl;
    return 0;
}
```
Dette vil skrive ut alderen til standard error, og resultatet vil se slik ut:
```
Alder: 35
```
Som du kan se, bruker vi uttrykket "std::endl" for å legge til linjeskift etter utskriften. Dette er for å få en ryddigere utskrift.

Deep Dive:
Bruken av standard error går tilbake til den tidlige utviklingen av datamaskiner, der standard output ofte ble reservert for dataprosesseringsutskrifter. Men etter hvert som utviklingen av operativsystemer og programmeringspråk har skjedd, har standard error blitt en viktig del av feilhåndtering og debugging. Alternativet til å skrive til standard error er å sende feilmeldinger til standard output, men dette kan føre til at viktig informasjon blir oversett i en stor mengde utskrifter. Implementeringen av standard error er forskjellig fra operativsystem til operativsystem, men i C++ er det vanligvis en del av standard biblioteker.

Se også:
- [Standard Error C++ Reference](https://en.cppreference.com/w/cpp/io/cerr)
- [What's the difference between std::err and std::cout?](https://stackoverflow.com/questions/55903853/what-is-the-difference-between-stdcout-and-stdcerr)