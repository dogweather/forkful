---
title:    "C++: Søke og erstatte tekst"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere bruker tekstbehandling hver dag for å skrive koder, tekstfiler og dokumentasjon. Men hva skal man gjøre når man vil endre noe i et stort dokument? Eller hvis man ønsker å endre ett ord i alle filene i et prosjekt? Det er her søk og erstatt-funksjonen kommer inn i bildet.

## Hvordan

Først og fremst, hva betyr det å søke og erstatte tekst? Det er en enkel måte å finne og erstatte et bestemt stykke tekst med et annet. Dette kan spare deg for mye tid og frustrasjon!

La oss si at vi har en tekstfil med følgende innhold:

```
Dette er en tekstfil som jeg skal bruke som et eksempel.
```
Vi ønsker å endre "tekstfil" til "dokument". Dette kan enkelt gjøres ved å bruke søk og erstatt-funksjonen i C++. Her er et eksempel på hvordan det kan gjøres:

```
#include <iostream>
#include <fstream> //Funksjonalitet for å lese og skrive til filer
#include <string>

using namespace std;

int main() {
    ifstream innfil("eksempel.txt"); //Åpner filen for lesing
    ofstream utfil("ny_fil.txt"); //Oppretter en ny fil for å skrive til
    string linje;

    while (getline(innfil, linje)) { //Leser linje for linje
        size_t pos = linje.find("tekstfil"); //Finner posisjonen til ordet "tekstfil"

        if (pos != string::npos) { //Hvis ordet finnes i linjen
            linje.replace(pos, 8, "dokument"); //Erstatter ordet med "dokument"
        }
        
        utfil << linje << endl; //Skriver linjen til ny fil
    }

    innfil.close(); //Lukker filene
    utfil.close();

    return 0;
}
```

Etter at koden har kjørt, vil den nye filen `ny_fil.txt` se slik ut:

```
Dette er en dokument som jeg skal bruke som et eksempel.
```

Her er noen ting det er verdt å merke seg:
- `size_t` er en datatype som brukes til å representere lengden på en tekststreng
- `find()` funksjonen returnerer posisjonen til det søkte ordet i en tekststreng, eller `string::npos` hvis det ikke finnes
- `replace()` funksjonen erstatter et bestemt antall tegn fra en gitt posisjon med en annen tekst
- `getline()` funksjonen brukes til å lese en hel linje fra en fil

Du kan også søke og erstatte tekst i en tekstredigerer som Visual Studio Code eller Notepad++. Det finnes også flere dedikerte søk og erstatt-programmer tilgjengelig på nettet.

## Dypdykk

Søk og erstatt-funksjonen kan brukes til mye mer enn bare å endre enkeltord i en tekstfil. Du kan også søke etter et mønster og erstatte med en annen tekst, eller til og med bruke regulære uttrykk. Dette kan være svært nyttig når du jobber med store datasett eller koder. Det er også lurt å være oppmerksom på forskjellen mellom en enkel og en global søk og erstatt. En enkel søk og erstatt vil kun endre det første tilfellet av det søkte ordet, mens en global søk og erstatt vil endre alle tilfeller.

## Se også

- [C++ Dokumentasjon: `string::find()` og `string::replace()`](https://en.cppreference.com/w/cpp/string/basic_string/find)
- [Visual Studio Code søk og erstatt dokumentasjon](https://code.visualstudio.com/docs/editor/codebasics#_find-and-replace)
- [Notepad++ søk og erstatt tutorial](https://www.makeuseof.com/tag/notepad-super-fast-find-replace-tutorial/)