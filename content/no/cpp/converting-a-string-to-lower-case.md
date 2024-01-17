---
title:                "Konvertere en streng til små bokstaver"
html_title:           "C++: Konvertere en streng til små bokstaver"
simple_title:         "Konvertere en streng til små bokstaver"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver er en vanlig programmeringsoppgave som innebærer å endre alle store bokstaver til små bokstaver i en tekststreng. Dette gjøres vanligvis for å sikre enhetlighet i tekstbehandling og sammenligninger av datasystemer.

##Slik gjør du:
Hvis du skriver i C++, kan du bruke transform-funksjonen for å konvertere en streng til små bokstaver ved å bruke standardbiblioteket «algorithm». Du kan også lage en egen funksjon som tar inn en streng og returnerer en kopi der alle store bokstaver er konvertert til små.

```C++
#include <algorithm>
#include <string>
using namespace std;

// Konverterer en streng til små bokstaver
string toLowerCase(string s) {
  transform(s.begin(), s.end(), s.begin(), ::tolower);
  return s;
}
```

Et eksempel på output med denne funksjonen:
```
string s = "HEI PÅ DEG";
cout << toLowerCase(s); // hei på deg
```

## Dypdykk:
Konvertering av en streng til små bokstaver er en vanlig oppgave ved datahåndtering og tekstbehandling. Det har tradisjonelt blitt gjort ved å bruke funksjoner som lowercase() eller tolower(). Imidlertid er disse funksjonene ofte avhengige av lokale innstillinger og kan gi ulike resultater avhengig av operativsystem og språkinnstillinger. Derfor anbefales det å bruke transform-funksjonen for en mer pålitelig og portabel løsning.

## Se også:
- [C++ standardbiblioteket «algorithm»](https://www.cplusplus.com/reference/algorithm/)
- [Diskusjon om å konvertere en streng til små bokstaver](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)