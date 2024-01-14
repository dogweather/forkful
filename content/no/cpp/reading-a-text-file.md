---
title:                "C++: Å lese en tekstfil."
simple_title:         "Å lese en tekstfil."
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du skriver kode i C++, vil du før eller senere måtte lese en tekstfil. Det kan være for å behandle store mengder data, holde informasjon lagret utenfor selve programmet, eller for å lese konfigurasjonsfiler. Uansett årsak, er kunnskap om hvordan man leser en tekstfil en viktig del av programmeringsverden.

## Hvordan å lese en tekstfil i C++

Det finnes flere måter å lese en tekstfil i C++, men den enkleste metoden er å bruke filbehandleren `ifstream`. Først må du inkludere `fstream` biblioteket i koden din. Deretter må du opprette en `ifstream`-objekt ved å passere navnet på filen du ønsker å lese som en parameter.

```C++
#include <fstream>

ifstream fil ("tekstfil.txt");
```

Nå kan du bruke forskjellige funksjoner for å lese data fra tekstfilen. For eksempel kan du lese en linje ved hjelp av `getline`-funksjonen og deretter skrive ut den leste linjen til konsollen.

```C++
string linje;

// Leser en linje fra tekstfilen og lagrer den i "linje"-variabelen
getline(fil, linje);

// Skriver ut den leste linjen til konsollen
cout << linje << endl;
```

Du kan også lese data fra tekstfilen ved hjelp av en `while`-løkke og `eof`-funksjonen. `eof` returnerer `true` når slutten av filen er nådd.

```C++
string linje;

// Utfører en løkke så lenge "fil" ikke har nådd slutten av filen
while (!fil.eof()) {
    // Leser en linje fra tekstfilen og lagrer den i "linje"-variabelen
    getline(fil, linje);

    // Skriver ut den leste linjen til konsollen
    cout << linje << endl;
}
```

## Dypdykk

Når du skal lese en tekstfil i C++, er det viktig å være oppmerksom på forskjellige faktorer som kan påvirke resultatet. For eksempel kan tegnsettet til tekstfilen være forskjellig fra det som er støttet av C++. I så fall må du kanskje konvertere tegnene ved hjelp av `setlocale`-funksjonen.

Det er også viktig å sørge for at tekstfilen faktisk finnes før du prøver å lese den. Du kan gjøre dette ved hjelp av `fail`-funksjonen, som returnerer `true` hvis filen ikke ble åpnet riktig.

Sørg også for å lukke filbehandleren ved å kalle `fil.close()` etter at du er ferdig med å lese tekstfilen.

## Se også

- [C++ Filbehandling - Tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [C++ Filbehandling - cplusplus.com](http://www.cplusplus.com/doc/tutorial/files/)