---
title:    "C++: Uttrekking av undertekster"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere har støtt på behovet for å hente subtstrenger fra en større tekststreng. Dette kan være nyttig for å behandle data eller for å søke i en tekst etter en spesifikk informasjon. Det kan også være en del av en større algoritme eller funksjon. Uansett årsak, er evnen til å ekstrahere substrings viktig i programmering.

## Slik gjør du det

For å ekstrahere substrings i C++, må man bruke noen innebygde funksjoner som finnes i C++-biblioteket. Først må man inkludere "string" biblioteket ved å bruke #include kommandoen:

```C++
#include <string>
```

Deretter kan man bruke funksjonen "substr()" som tar inn to parametere - startindeksen og lengden på substringen. Her er et eksempel:

```C++
// Opprett en tekststreng
string tekst = "Dette er en tekststreng";

// Ekstraher en substring fra startindeks 11 og lengde 5
string substring = tekst.substr(11, 5);

// Skriv ut resultatet
cout << substring << endl;

// Output: "tekst"
```

Man kan også bruke funksjonen "find()" til å finne indeksen til en spesifikk substring, og deretter bruke "substr()" til å ekstrahere den. Her er et eksempel:

```C++
// Finn startindeksen til substringen "er"
int indeks = tekst.find("er");

// Ekstraher en substring fra startindeksen og lengden 6
substring = tekst.substr(indeks, 6);

// Skriv ut resultatet
cout << substring << endl;

// Output: "er en"
```

## Dypdykk

I tillegg til å bruke de innebygde funksjonene "substr()" og "find()", kan man også implementere sin egen substring-funksjon. Dette er nyttig dersom man ønsker å tilpasse funksjonen til et spesifikt formål eller ønsker å forbedre ytelsen. Om man bruker en "for loop" til å søke gjennom tekststrengen og hente ut karakterer i en gitt range, kan man lage en funksjon som dette:

```C++
string substring(string tekst, int start, int lengde){

    // Opprett et objekt for den ekstraherte substringen
    string substring = "";

    // Gå gjennom hver karakter i teksten
    for (int i = start; i < start + lengde; i++){
        
        // Legg til karakteren i substringen
        substring += tekst[i];
    }

    // Returner den ekstraherte substringen
    return substring;
}

// Eksempel på bruk
string resultat = substring("Dette er en tekststreng", 11, 5);

// Output: "tekst"
```

## Se også

- [String-funksjoner i C++](https://www.geeksforgeeks.org/cpp-string-class-and-its-applications/)
- [Hva er substrings](https://en.wikipedia.org/wiki/Substring)