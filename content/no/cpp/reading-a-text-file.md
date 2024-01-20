---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Å lese en tekstfil i programmering handler om å hente informasjon lagret i tekstformat, som ofte innebærer symboler, tall, ord og setninger. Programmerere gjør dette for å behandle, manipulere, og analysere data som er lagret i filer.

## Slik Gjør Du:

Grunnleggende kodeeksempel for å lese en tekstfil i C++ er:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ifstream file("eksempel.txt");
    std::string str;
    
    while (std::getline(file, str)) {
        std::cout << str << "\n";
    }

    file.close();
    return 0;
}
```

I dette eksempelet er "eksempel.txt" navnet på filen vi ønsker å lese. For hver linje i filen, printer programmet linjen på skjermen.

## Dypdykk

Historisk sett, lese og skrive til filer har alltid vært fundamentalt i programmering, særlig for å lagre og hente data. C++ inkluderer effektive biblioteker og funksjoner for filhåndtering.

Alternativer for å lese en tekstfil i C++ inkluderer bruk av `fscanf()`, `fgetc()`, eller `fread()` funksjoner fra C standardbibliotek. Dine valg kommer an på dine spesifikke behov, dataformat og ytelseskrav.

Når det kommer til selve implementeringen, så åpner `ifstream` konstruktøren filen, og `getline()` funksjonen brukes til å lese filen linje for linje. Det er viktig å lukke filen etter bruk ved å kalle `close()` funksjonen for å frigjøre ressurser.

## Se Også

For mer detaljeret informasjon om filhåndtering i C++, se følgende linker:

1. CPlusPlus.com's filhåndtering tutorial: [Link](http://www.cplusplus.com/doc/tutorial/files/)
2. StackOverflow diskusjon om lese en fil i C++: [Link](https://stackoverflow.com/questions/7868936/read-file-line-by-line)
3. GeeksForGeeks guide til filhåndtering: [Link](https://www.geeksforgeeks.org/file-handling-c-classes/)