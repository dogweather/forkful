---
title:                "Å skrive en tekstfil"
html_title:           "C++: Å skrive en tekstfil"
simple_title:         "Å skrive en tekstfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Hvis du er en programmerer, er det en god sjanse for at du vil skrive en tekstfil på et tidspunkt. En tekstfil er en fil som inneholder tekst og kan leses og endres av mennesker og datamaskiner. Programmere skriver tekstfiler for å lagre data eller utdatere informasjon på et varig lagringssted.

## Hvordan å:
I C++, kan du lage og skrive til en tekstfil ved å følge disse enkle trinnene:

```
// include nødvendige biblioteker
#include <iostream>
#include <fstream>

int main() {
  // åpne en tekstfil for å skrive til
  std::ofstream file("mitt_filnavn.txt");

  // skrive tekst til filen
  file << "Dette er min første tekstfil i C++!" << std::endl;

  // lukke filen
  file.close();
  
  return 0;
}
```

##### Output:
```
Dette er min første tekstfil i C++!
```

## Dykke dypere:
Å skrive tekstfiler har vært en viktig del av programmering siden de tidlige dagene. Før det var grafiske grensesnitt, var tekstfiler det eneste formatet for å lagre og utveksle data. Alternativene til å skrive tekstfiler er å bruke en database eller en binær fil, men tekstfiler er ofte foretrukket på grunn av deres enkelhet og lesbarhet. Når du skriver en tekstfil, må du være oppmerksom på formatering og encoding for å sikre at filen kan leses på ulike enheter og systemer.

## Se også:
- [C++ ofstream dokumentasjon](http://www.cplusplus.com/reference/fstream/ofstream/)
- [Wikipedia siden om Tekstfiler](https://en.wikipedia.org/wiki/Text_file)
  []