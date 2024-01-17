---
title:                "Stor bokstavering av en streng"
html_title:           "C++: Stor bokstavering av en streng"
simple_title:         "Stor bokstavering av en streng"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av en streng er en prosess der den første bokstaven i hvert ord i en setning blir gjort stor. Dette gjør teksten lettere å lese og følge for både mennesker og datamaskiner.

Programmerere bruker kapitalisering for å gjøre koden mer leselig, spesielt når det kommer til variabelnavn og kommentarer. Det bidrar til å tydeliggjøre koden og gjøre den enklere å forstå.

## Slik gjør du:
Kapitalisering i C++ kan gjøres ved å bruke transform-funksjonen og toupper-funksjonen fra STL-biblioteket. Se eksempelkoden nedenfor for å se hvordan det gjøres:

```
// Eksempel på kapitalisering av en streng i C++
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main() {
    string sentence = "dette er en test";
    transform(sentence.begin(), sentence.end(), sentence.begin(), ::toupper);
    cout << sentence << endl;
    return 0;
}
```

Dette vil produsere følgende utgang:

`DETTE ER EN TEST`

## Dypdykk:
Historisk sett ble kapitalisering brukt i programmene til tidlige datamaskiner, som bare taklet store bokstaver. I dag brukes det primært for å gjøre koden mer lesbar og strukturert.

Alternativene til å bruke STL-funksjoner for å kapitalisere en streng inkluderer å lage en egen funksjon eller å bruke en tredjepartsbibliotek.

Implementeringsdetaljer for kapitalisering kan variere avhengig av programmeringsspråket, men konseptet og bruken av STL-biblioteket forblir det samme.

## Se også:
- [STL-biblioteket i C++](https://www.geeksforgeeks.org/the-c-standard-template-library-stl/)
- [Implementasjon av kapitalisering i Java](https://www.techiedelight.com/capitalize-first-letter-of-each-word-string-java/)