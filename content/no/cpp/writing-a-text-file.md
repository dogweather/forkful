---
title:    "C++: Å skrive en tekstdokument"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere vil på et eller annet tidspunkt trenge å lagre data på en enkel og organiserbar måte. Å skrive en tekstfil i C++ er en effektiv måte å oppnå dette på.

## Slik går du frem

For å skrive en tekstfil i C++, trenger du å inkludere "fstream" biblioteket og åpne en "ofstream" objekt med navnet på filen du ønsker å skrive til. Deretter kan du bruke vanlig << operator for å skrive ønsket informasjon til filen, som i følgende eksempel:

```C++
#include <iostream>
#include <fstream>

int main()
{
    std::ofstream min_fil("tekstfil.txt");

    min_fil << "Dette er en eksempeltekst";

    min_fil.close();

    return 0;
}
```

Denne koden åpner en tekstfil med navnet "tekstfil.txt" og skriver teksten "Dette er en eksempeltekst" til filen. Til slutt lukker koden filen. Du kan også bruke "ifstream" objektet for å lese data fra en fil.

## Dykk dypere

For å skrive mer avansert informasjon til en tekstfil, kan du bruke manipulatorer og formateringsinstruksjoner for å style teksten. Du kan også bruke "seekp" og "tellp" funksjonene for å flytte leseposisjonen til en bestemt plass i filen. Det kan også være nyttig å sjekke for eventuelle feil under skriving eller lesing av en fil.

## Se også

- [C++ ofstream dokumentasjon](https://www.cplusplus.com/reference/fstream/ofstream/)
- [C++ ifstream dokumentasjon](https://www.cplusplus.com/reference/fstream/ifstream/)
- [C++ seekp dokumentasjon](https://www.cplusplus.com/reference/fstream/ofstream/seekp/)
- [C++ tellp dokumentasjon](https://www.cplusplus.com/reference/fstream/ofstream/tellp/)