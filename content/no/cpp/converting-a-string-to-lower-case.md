---
title:                "C++: Konvertere en streng til små bokstaver"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i C++ programmering. Dette er nyttig når man for eksempel skal sammenligne to strenger, da man ofte ønsker å ignorere forskjeller i bokstavstørrelse og fokusere på selve innholdet i strengene.

# Hvordan

For å konvertere en streng til små bokstaver i C++, kan man bruke både standardbiblioteket og enkle metoder. La oss se på et eksempel:

```C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    // Opprett en streng med store bokstaver
    string str = "Dette ER En TEkST";

    // Bruk transform() funksjonen fra <algorithm> til å konvertere strengen til små bokstaver
    transform(str.begin(), str.end(), str.begin(), ::tolower);

    // Skriv ut den konverterte strengen
    cout << str << endl;

    return 0;
}
```

Output:

```
dette er en tekst
```

I dette eksempelet bruker vi funksjonen `transform()` fra `<algorithm>` biblioteket til å konvertere alle bokstavene i strengen til små bokstaver. Denne funksjonen tar inn tre parametere: startpunktet for strengen, slutt-tpunktet og hvor den skal skrive de konverterte bokstavene. Her bruker vi også `::tolower` som en lambda-funksjon som konverterer hver enkelt bokstav til små bokstaver.

Det finnes også en enklere metode for å oppnå samme resultat:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Opprett en streng med store bokstaver
    string str = "Dette ER En TEkST";

    // Loop gjennom strengen og konverter hver enkelt bokstav til små bokstaver
    for (int i = 0; i < str.length(); i++) {
        str[i] = tolower(str[i]);
    }

    // Skriv ut den konverterte strengen
    cout << str << endl;

    return 0;
}
```

# Dypdykk

I tillegg til å konvertere til små bokstaver, finnes det også andre nyttige funksjoner for bokstavkonvertering i C++. For eksempel kan man bruke `toupper()` for å konvertere til store bokstaver, og `islower()` og `isupper()` for å sjekke om en bokstav er henholdsvis liten eller stor.

Det er også viktig å være oppmerksom på at bokstaver kan variere avhengig av språk. For eksempel vil bokstaven "å" ha forskjellige betydninger i norsk og svensk, og dermed også forskjellige verdier i C++. Det kan derfor være lurt å bruke Unicode eller UTF-8 for å sikre at alle bokstaver blir konvertert riktig.

# Se også

- [String to Lower/Upper Case in C++](https://www.geeksforgeeks.org/string-to-lower-upper-case-in-cpp/)
- [C++ Transform Function](https://www.geeksforgeeks.org/transform-function-in-c-stl/)

Takk for at du leste denne artikkelen! Vi håper den har vært nyttig for deg i din C++ programmering. Lykke til videre med å konvertere strenger til små bokstaver!