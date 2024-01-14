---
title:    "C++: Utvinning av delstrenger"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Det å trekke ut substringer, eller delstrenger, fra en større tekst er en nyttig teknikk når du jobber med å manipulere tekst i et program. Dette kan være nyttig for å filtrere og bearbeide data, samt å lage mer dynamiske og interaktive tekstbaserte applikasjoner.

## Slik gjør du det

For å trekke ut en delstreng fra en tekst, kan du bruke funksjonen `substr()`. Denne funksjonen tar to argumenter: startposisjonen og lengden på delstrengen. Her er et enkelt eksempel som viser hvordan du kan bruke `substr()`:

```C++
#include <iostream>
using namespace std;

int main() {
    string tekst = "Dette er en lang tekst";
    // Trekker ut delstreng fra posisjon 5 til og med 8
    string resultat = tekst.substr(5, 4);
    // Skriver ut resultatet på skjermen
    cout << resultat << endl;

    return 0;
}
```

Dette vil resultere i utskriften "er en" på skjermen. Fra posisjon 5 i teksten, som er "e", blir de neste 4 bokstavene trekt ut.

Det er viktig å merke seg at startposisjonen i `substr()`funksjonen begynner på 0, ikke 1. Dette betyr at for å trekke ut første del av teksten, må du bruke startposisjonen 0 og lengden 1.

## Dykk dypere

Når du arbeider med substrings, er det viktig å være klar over at de er en del av den opprinnelige teksten. Dette betyr at endringer i substrings vil påvirke den opprinnelige teksten. For eksempel:

```C++
string tekst = "Dette er en lang tekst";
string resultat = tekst.substr(5, 4);
// Endrer første bokstaven i resultatet til stor bokstav
resultat[0] = 'E';
cout << tekst << endl;
```

Dette vil resultere i utskriften "Dette Er en lang tekst" på skjermen. Den første bokstaven i delstrengen "er en" ble endret til stor bokstav, og dette påvirket også den opprinnelige teksten.

En annen ting å merke seg er at lengden på substrings også kan påvirke resten av teksten. Hvis du for eksempel har en tekst "Hva skjer?" og trekker ut "Hva", vil resten av teksten bli forkortet til " skjer?". Dette kan være viktig å tenke på når du arbeider med substrings.

## Se også

* [cplusplus.com reference for substr() function](http://www.cplusplus.com/reference/string/string/substr/)
* [Tutorialspoint article on substrings in C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)