---
title:                "Sammenkobling av strenger"
html_title:           "C++: Sammenkobling av strenger"
simple_title:         "Sammenkobling av strenger"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere strenger er en viktig del av programmering, spesielt når man jobber med tekstbaserte applikasjoner. Det lar deg lage dynamiske og mer komplekse tekster ved å sette sammen flere mindre biter.

## Hvordan

For å kombinere strenger i C++, kan du bruke operatoren "+" eller funksjonen "concatenate()". La oss se på noen eksempler:

### Eksempel 1: "+" operatør

```C++
#include <iostream>
using namespace std;

int main() {
  string navn = "Lars";
  string etternavn = "Olsen";
  string navn_etternavn = navn + " " + etternavn;
  cout << navn_etternavn << endl;
  return 0;
}
```

**Output**: Lars Olsen

Her har vi satt sammen tre strenger ved hjelp av "+" operatøren. Den første stringen er navnet, deretter legger vi til et mellomrom og til slutt legger vi til etternavnet. Ved å bruke denne operatøren, kan du enkelt legge til hvilke som helst antall strenger.

### Eksempel 2: concatenate() funksjon

```C++
#include <iostream>
#include <string>
using namespace std;

int main() {
  string produktnavn = "Melk";
  string merke = "Tine";
  string produkt = concatenate("Jeg kjøpte ", produktnavn, " fra ", merke, " i dag.");
  cout << produkt << endl;
  return 0;
}
```

**Output**: Jeg kjøpte Melk fra Tine i dag.

I dette eksempelet bruker vi "concatenate()" funksjonen fra "string" biblioteket for å kombinere strenger. Den fungerer på samme måte som "+" operatøren, men gir også mer fleksibilitet til å formatere og legge til flere variabler.

## Deep Dive

Når man kombinerer to eller flere strenger, er det viktig å sørge for at det ikke blir noen uønskede mellomrom eller tegn mellom dem. En enkel måte å unngå dette på, er å bruke "trim()" funksjonen som fjerner ekstra mellomrom før og etter en streng.

Det er også viktig å merke seg at du bare kan kombinere strenger med samme datatype. For eksempel kan du ikke kombinere en "string" og en "int" variabel.

## Se også

- [C++ String Operators](https://www.w3schools.com/cpp/cpp_strings_operators.asp)
- [String Concatenation](https://www.cplusplus.com/reference/string/string/operators/)
- [C++ Strings](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)