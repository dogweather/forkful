---
title:    "C++: Opprettelse av midlertidig fil"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

I programmering, er det ofte nødvendig å opprette midlertidige filer for å lagre midlertidige data. Dette kan være nyttig når man jobber med store datamengder eller prosesser som krever midlertidig lagring av informasjon. Midlertidige filer slettes vanligvis automatisk når programmet har fullført sin oppgave, noe som bidrar til å redusere rotet på datamaskinen og beholde god systemytelse.

## Hvordan

For å opprette en midlertidig fil i C++, kan man bruke funksjonen "tmpfile()", som returnerer en peker til den midlertidige filen. Her er et eksempel på hvordan man kan bruke denne funksjonen:

```C++
#include <cstdio>
#include <iostream>

int main() {
    FILE * file = tmpfile();
    if (file != NULL) {
        std::cout << "Midlertidig fil opprettet!\n";
        fputs("Dette er en midlertidig fil.", file);
        rewind(file);

        char buffer[20];
        while (fgets(buffer, sizeof(buffer), file) != NULL) {
            std::cout << "Innholdet i filen: " << buffer;
        }
        fclose(file);
    }
}
```

I dette eksempelet opprettes det en midlertidig fil ved hjelp av "tmpfile()" og deretter skrives en tekststreng til filen ved hjelp av "fputs()". Deretter brukes "rewind()" til å tilbakestille posisjonen i filen og "fgets()" til å lese innholdet og skrive det ut på skjermen. Til slutt lukkes filen ved å bruke "fclose()".

Output av dette eksempelet vil være:

```
Midlertidig fil opprettet!
Innholdet i filen: Dette er en midlertidig fil.
```

## Dypdykk

I de fleste tilfeller vil det være nok å bruke "tmpfile()" for å opprette midlertidige filer. Men for mer avanserte behov kan man bruke funksjonen "mkstemp()", som lar brukeren spesifisere et filnavn og returnerer en fil-deskriptor i tillegg til navnet på den opprettede filen. Dette kan være nyttig hvis man for eksempel vil endre rettighetene til den midlertidige filen. Et annet alternativ er å bruke funksjonen "mkdtemp()", som oppretter en midlertidig mappe istedenfor en fil.

## Se også

- [C++ filbehandling](https://www.dinkurs.no/programmering/ta-opp-leksjoner/c-plus-plus/filbehandling)
- [C++ dokumentasjon for tmpfile()](https://www.cplusplus.com/reference/cstdio/tmpfile/)
- [Artikkel om bruk av midlertidige filer i programmering](https://www.techwalla.com/articles/the-purpose-of-temporary-files-in-a-computer)