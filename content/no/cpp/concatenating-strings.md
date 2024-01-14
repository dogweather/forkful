---
title:    "C++: Sammenføyning av strenger"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å koble sammen strenger er en nyttig funksjon når du ønsker å kombinere eller bygge en lengre streng fra mindre deler. Dette kan være nyttig for å lage meldinger eller utskrifter som krever både variabler og tekst.

## Hvordan

For å koble sammen strenger i C++, bruker vi operatoren "+" eller "append()" funksjonen. La oss se på noen eksempler:

```C++
string navn = "Hans";

//Koble sammen to strenger ved hjelp av operatoren "+"
string velkomst = "Hei " + navn;

//Bruk "append()" funksjonen for å legge til tekst til en eksisterende streng
velkomst.append(", velkommen!");

//Utskrift: Hei Hans, velkommen!
cout << velkomst << endl;
```

I dette eksemplet kombinerer vi strenger for å lage en velkomstmelding til personen med navnet "Hans". Vi bruker "+ "operatoren for å koble sammen navnet med "velkommen!" strengen, og deretter bruker "append()" funksjonen for å legge til "velkommen!" på slutten av vår velkomstmelding.

Vi kan også kombinere flere strenger på en gang ved å bruke operatoren "+". La oss se på et annet eksempel:

```C++
string navn = "Marie";
string yrke = "lærer";

//Koble sammen tre strenger
string presentasjon = navn + " er en " + yrke;

//Utskrift: Marie er en lærer
cout << presentasjon << endl;
```

## Dykk dypere

I disse eksemplene bruker vi kun operatoren "+", men i virkeligheten bruker C++ en mer effektiv metode for å koble sammen strenger. Når vi bruker "+" operatoren, oppretter vi faktisk en ny streng hver gang vi kobler sammen to strenger, noe som kan føre til unødvendig bruk av minne.

For å forbedre ytelsen, bruker C++ en klasse kalt "stringstream" som lar deg bygge en streng uten å opprette flere kopier. Her er et eksempel på hvordan vi kan bruke "stringstream" for å koble sammen strenger:

```C++
#include <sstream>

string navn = "Carl";
string yrke = "ingeniør";

//Opprett et stringstream objekt
stringstream ss;

//Bygg en streng ved hjelp av stringstream
ss << navn << " er en " << yrke;

//Hent verdien fra stringstream og lagre den i en strengvariabel
string presentasjon = ss.str();

//Utskrift: Carl er en ingeniør
cout << presentasjon << endl;
```

Dette kan være nyttig når du må koble sammen en stor mengde strenger, da dette vil redusere antall kopieringsoperasjoner og forbedre programmet ditt sin ytelse.

## Se også

- [String concatenation in C++](https://www.geeksforgeeks.org/string-concatenation-in-c/)
- [String streams in C++](https://www.geeksforgeeks.org/string-stream-strings-streams/)
- [C++ Reference - String](https://www.cplusplus.com/reference/string/string/)