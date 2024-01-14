---
title:    "C++: Å finne lengden av en streng"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng (string) er en vanlig oppgave i C++ programmering. Det er viktig å kunne gjøre dette for å håndtere og analysere tekstbaserte data, som for eksempel brukerinndata eller tekstfiler. Ved å vite lengden på en streng kan du også utføre forskjellige operasjoner, som å skrive ut en bestemt del av en streng eller sammenligne to strenger.

## Hvordan
For å finne lengden på en streng i C++, bruker vi funksjonen `length()`. Denne funksjonen er tilgjengelig fra standardbiblioteket `string` og tar inn en streng som argument. Under er et eksempel på hvordan vi kan bruke denne funksjonen:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

    string tekst = "Jeg liker å programmere!";
    int lengde = tekst.length();
    cout << "Lengden på strengen \"Jeg liker å programmere!\" er " << lengde << " tegn." << endl;

    return 0;
}
```
Dette vil gi følgende output:

```
Lengden på strengen "Jeg liker å programmere!" er 25 tegn.
```

En annen måte å finne lengden på en streng på er å bruke funksjonen `size()`. Denne funksjonen fungerer på samme måte som `length()` og kan brukes som et alternativ.

Det er også verdt å merke seg at lengden på en streng er forskjellig fra antall tegn (characters) den inneholder. Dette skyldes at enkelte tegn kan ta opp mer enn én byte i minnet. Derfor, dersom du ønsker å finne antall tegn i en streng, bør du bruke funksjonen `size()` som tar hensyn til dette.

## Dypdykk
Som nevnt tidligere, er lengden på en streng forskjellig fra antall tegn. Dette kan være en viktig faktor å tenke på dersom du jobber med flerspråklige strenger som inneholder spesielle tegn eller emojis. I slike tilfeller kan det være lurt å konvertere strengen til en annen encoding (tegnsett) før du finner lengden for å få et korrekt resultat.

En annen ting å være oppmerksom på er at funksjonen `length()` ikke teller med null-tegnet (null character) som markerer slutten av en streng. Dersom du trenger å inkludere dette tegnet i lengden, kan du bruke funksjonen `size()` i stedet.

## Se Også
- [C++ String Functions](https://www.geeksforgeeks.org/c-string-functions/) av GeeksforGeeks
- [C++ String Class](https://www.programiz.com/cpp-programming/string) av Programiz
- [C++ String Length](https://www.w3schools.com/cpp/cpp_strings_length.asp) av W3Schools