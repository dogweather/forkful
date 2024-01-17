---
title:                "Å lese en tekstfil"
html_title:           "C++: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Leser en tekstfil betyr å lese og tolke innholdet i en fil som er lagret på datamaskinen din. Programmere gjør dette for å kunne lese data fra en fil og behandle det i deres programvare.

## Hvordan:
```C++
#include <iostream>
#include <fstream>
using namespace std;

int main() {
  // Åpne filen med ifstream og pass på å åpne en eksisterende fil
  ifstream file("tekstfil.txt");

  // Sjekke om filen er åpen og klar for lesing
  if (file.is_open()) {
    // Lage en variabel for å lagre data fra filen
    string data;
    // Lese en linje fra filen og lagre den i data variabelen
    getline(file, data);
    // Skriv ut dataen som er lest
    cout << "Data lest fra fil: " << data;
    // Lukk filen
    file.close();
  }
  return 0;
}
```
Eksempel på utskrift: 
Data lest fra fil: Hei, dette er en tekstfil!

## Dypdykk:
Historisk, ble tekstfiler brukt som primære måten å lagre data på datamaskiner. Alternativene er å bruke binærfiler eller databaser. Når du leser en tekstfil, må programmøren ta hensyn til forskjellige tegnkodinger og tekstformater for å kunne tolke og behandle dataen. Implementeringen av å lese en tekstfil innebærer å åpne filen, lese linje for linje og deretter behandle dataen som er lest. 

## Se også:
Les mer om standard C++ funksjoner for å lese og skrive til filer her: https://www.cplusplus.com/doc/tutorial/files/