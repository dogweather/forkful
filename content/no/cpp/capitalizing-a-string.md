---
title:    "C++: Storing en streng"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger kan det være nyttig å endre formateringen av en tekststreng i et C++ program. En vanlig manipulasjon er å gjøre den første bokstaven i hvert ord til en stor bokstav, også kjent som kapitalisering. Dette kan være nyttig for å gjøre output mer leselig eller for å følge en spesifikk formatertingsstandard. I denne bloggposten vil vi utforske hvordan man kan kapitalisere en tekststreng i C++.

## Hvordan gjøre det

For å kapitalisere en tekststreng i C++ kan vi bruke funksjonen `toupper()` fra standardbiblioteket `<cctype>`. Denne funksjonen tar inn en bokstav og returnerer den samme bokstaven, men i stor bokstav. Vi kan gjenta denne prosessen for hver bokstav i tekststrengen ved å bruke en `for`-løkke.

Her er en kodet kodeblokk som viser hvordan man kan gjøre dette:

```C++
#include <iostream>
#include <cctype> 

using namespace std;

int main() {
  string tekststreng = "dette er en tekststreng";

  for (int i = 0; i < tekststreng.length(); i++) {
    tekststreng[i] = toupper(tekststreng[i]); // bruker toupper() for å kapitalisere hver bokstav
  }

  cout << tekststreng << endl;

  return 0;
}
```

Koden vil gi følgende output:

```C++
DETTE ER EN TEKSTSTRENG
```

Hvis du vil kapitalisere en tekststreng uten å endre originalen, kan du heller lagre den kapitaliserte strengen i en ny variabel.

## Dypdykk

Nå som vi har sett på hvordan man kan kapitalisere en tekststreng i C++, la oss utforske litt dypere om funksjonen `toupper()` og hvordan den fungerer. Denne funksjon er definert i standardbiblioteket `<cctype>` og tar inn en enkelt `char` som parameter. Den sjekker om bokstaven er en liten bokstav eller ikke ved å bruke ASCII-tabellen. Hvis bokstaven er en liten bokstav, returnerer den bokstaven i stor bokstav. Hvis bokstaven allerede er en stor bokstav, returnerer den den samme bokstaven.

Det er også en tilsvarende funksjon kalt `tolower()` som kan brukes til å gjøre bokstavene i en tekststreng til små bokstaver.

## Se også

- [C++: capitalize first character of a string](https://stackoverflow.com/questions/31102910/c-capitalize-first-character-of-a-string)
- [String - cppreference.com](https://en.cppreference.com/w/cpp/string/basic_string)
- [C++ Standardbibliotek: `<cctype>`](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [ASCII-tabellen](https://www.ascii-code.com/)