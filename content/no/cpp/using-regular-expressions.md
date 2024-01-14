---
title:    "C++: Å bruke regulære uttrykk"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor
Regular expressions, or "regex", er en kraftig verktøy for å søke og manipulere tekst i programmering. Ved å lære regex, kan du effektivisere søk og erstatning, og oppnå mer fleksibilitet i programmet ditt. 

## Hvordan Å
Ved å bruke C++, kan du enkelt integrere regex i kode for å utføre avansert tekstbehandling. Her er et eksempel på hvordan du kan bruke regex til å finne og erstatte tekst:

```C++
#include <iostream>
#include <regex>
using namespace std;

int main(){
  string input = "Hei, mitt navn er [navn]. Hva er ditt navn?";
  regex rgx("[navn]"); // regex-mønsteret vi vil finne
  string output = regex_replace(input, rgx, "Lena"); //erstatter [navn] med "Lena"
  cout << output << endl;
  return 0;
}
```

Output:

Hei, mitt navn er Lena. Hva er ditt navn?

Som du kan se, kan du bruke regex til å finne og erstatte spesifikke deler av en tekststreng. Dette er spesielt nyttig hvis du har en stor tekstfil eller en input som kan variere.

## Dypdykk
I tillegg til grunnleggende regex-søk og erstattning, kan du også bruke regex for mer avanserte funksjoner som validering, uttrykksmatching og subtraktering. Ved å ta deg tid til å lære regex, kan du utnytte sin fulle potensiale og spare mye tid og arbeid i programmeringen din. 

Det finnes også mange online verktøy og ressurser for å hjelpe deg med å bygge og teste regex-mønster. Disse kan være nyttige når du arbeider med mer komplekse tekstbehandlingsoppgaver. 

## Se Også
- [En guide til regex i C++](https://www.regular-expressions.info/cpp.html) 
- [Offisiell regex referanse for C++](https://en.cppreference.com/w/cpp/regex) 
- [Regex101 - et online verktøy for å teste og debugge regex](https://regex101.com/)