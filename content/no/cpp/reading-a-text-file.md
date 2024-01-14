---
title:    "C++: Lese en tekstfil"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Lesing og skriving til filer er en uomtvistelig viktig del av programmering. Å kunne lese en tekstfil i C++ kan være nyttig når du vil lagre og behandle store mengder data, eller når du vil lagre informasjon permanent på en måte som kan leses av andre programmer. Uansett hva dine mål er, vil du ha nytte av å lære hvordan man leser en tekstfil i C++.

## Hvordan

Vi skal nå se på et enkelt eksempel på hvordan man kan lese en tekstfil i C++. Vi vil bruke en for-løkke for å iterere gjennom hver linje i filen og skrive den ut på konsollen. Vi starter med å inkludere "fstream" biblioteket, som lar oss åpne og håndtere filer.

```C++
#include <fstream>
```

Deretter oppretter vi en instans av "ifstream" klassen, som lar oss åpne en fil for leseformål. Vi gir konstruktøren navnet på filen vi ønsker å lese som parameter. Husk å inkludere filbanen hvis filen ikke er i samme mappe som koden din.

```C++
ifstream minFil("tekstfil.txt");
```

Nå kan vi bruke input-strømmen til å lese filen linje for linje. Vi bruker getline-funksjonen og printer ut hver linje på konsollen. Dette gjør vi i en for-løkke som vil fortsette å lese filen helt til vi når slutten.

```C++
string linje; // Variabel for å lagre hver linje i

for (int i = 0; getline(minFil, linje); i++) {
    cout << linje << endl; // Skriver ut linjen
}
```

Etter at vi er ferdig med å lese filen, lukker vi input-strømmen og frigjør eventuelle ressurser som er brukt.

```C++
minFil.close(); // Lukker input-strømmen
```

La oss se på et eksempel på en tekstfil og hvordan konsollen vil se ut når vi bruker dette eksempelet.

**Tekstfil:**

```
Hello, world!
This is a sample text file.
```

**Konsolloutput:**

```
Hello, world!
This is a sample text file.
```

Som du kan se, er det enkelt å lese en tekstfil ved hjelp av C++. Det er også mulig å lese innholdet som tall, eller gjøre andre operasjoner på filen. Det er bare fantasien som setter grenser!

## Dypdykk

For å lese en tekstfil i C++, må vi forstå hvilke klasser og funksjoner som er involvert i prosessen. Først og fremst trenger vi "fstream" biblioteket, som lar oss håndtere filer. Vi kan bruke "ofstream" for skriverettigheter og "ifstream" for leserettigheter til filer. For å åpne en fil, bruker vi en konstruktør med filnavnet som parameter. I eksempelet vårt, brukte vi getline-funksjonen for å lese en linje om gangen. Denne funksjonen tar to parametre - strømobjektet og en variabel som vil lagre innholdet.

Lesing av tekstfiler kan også være mer kompleks avhengig av hvordan filen er strukturert og hva slags informasjon du vil lese. Det kan være nyttig å se på dokumentasjonen til "fstream" biblioteket eller søke etter mer avanserte eksempler på nettet.

## Se også

- [Lære å lese og skrive til filer i C++](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Menyen og Filesystem i standardbiblioteket i C++17](https://en.cppreference.com/w/cpp/filesystem)
- [Eksempler på lesing av tekstfiler i C++](https://www.programiz.com/cpp-programming/input-output)