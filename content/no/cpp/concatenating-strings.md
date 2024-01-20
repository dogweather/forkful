---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

# C++ Streng Sammenføyning: En Enkel Guide

Introduksjon til sammenføyning av strenger med C++, et viktig konsept for programmerere på alle nivåer.

## Hva & Hvorfor?

Streng sammenføyning er handlingen av å sette sammen to eller flere strenger. Dette er viktig for å opprette dynamisk tekst, manipulere data eller bruke brukerinput på interessante måter.

## Hvordan:

Her er enkel kode for å forklare hvordan vi kan sammenføye strenger:

```C++
#include <iostream>
#include <string>

int main() {
  std::string str1 = "Hei, ";
  std::string str2 = "verden!";
  std::string str3 = str1 + str2;

  std::cout << str3;
  return 0;
}
```
Output: `Hei, verden!` 

I eksemplet over legger vi sammen to strenger "Hei, " og "verden!" for å opprette en ny streng "Hei, verden!".

## Dyp Dykk:

**Historisk Kontekst**: C++ tillot sammenføyning av strenger via '+'-operatøren siden etableringen. Dette betyr ikke at alternativer ikke eksisterer.

**Alternativer**: I tillegg til "+" operatøren, kan kommandoene `string::append()` eller `stringstream` også brukes for å tilføye strenger:

```C++
std::string str1 = "Hei, ";
std::string str2 = "verden!";
str1.append(str2);  // str1 er nå "Hei, verden!"
```

eller

```C++
#include <sstream>

std::ostringstream sstm;
sstm << "Hei, " << "verden!";
std::string str3 = sstm.str();  // str3 er nå "Hei, verden!"
```

**Implementeringsdetaljer**: Når det gjelder effektivitet, er det litt forskjell mellom alternativene. '+'-operatøren kan skape midlertidige objekter hvis det brukes sammenkjedet, `string::append()` er mer effektiv i disse tilfellene. `stringstream` har overhead ved å håndtere flere typer data, men gir mer funksjonalitet.

## Se Også:

- [C++ Reference: string::append()](http://www.cplusplus.com/reference/string/string/append/)
- [StackOverflow: C++ String Concatenation](https://stackoverflow.com/questions/18892281/most-idiomatic-way-in-c-to-concatenate-strings)
- [C++ Stringstreams](http://www.cplusplus.com/reference/sstream/)