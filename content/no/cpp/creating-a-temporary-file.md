---
title:    "C++: Oppretting av midlertidig fil"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/cpp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette en midlertidig fil kan være nyttig når du jobber med programmering i C++. Dette tillater deg å lagre midlertidige data som ikke er nødvendige for din endelige kode, men som kan være nyttig for testing eller feilsøking.

## Slik gjør du det

For å opprette en midlertidig fil i C++, må du først inkludere fstream biblioteket. Deretter kan du bruke ofstream-funksjonen til å opprette en midlertidig filnavn. Et eksempel på kode som oppretter en midlertidig fil og skriver til den kan se slik ut:

```C++
#include <iostream>
#include <fstream> 

int main() { 
  // Oppretter en midlertidig fil 
  std::ofstream temp("midlertidigfil.txt"); 
  
  // Sjekker om filen er åpen 
  if(temp.is_open()){ 
    temp << "Dette er en midlertidig fil.\n"; 
    temp << "Denne teksten vil bli skrevet til filen."; 
    temp.close(); // Lukker filen 
  } else { 
    std::cout << "Kunne ikke åpne filen."; 
  } 
  return 0; 
} 
```

Når koden kjøres, vil den opprette en fil kalt "midlertidigfil.txt" og skrive teksten "Dette er en midlertidig fil. Denne teksten vil bli skrevet til filen." til den.

## Dykk dypere

Når du oppretter en midlertidig fil i C++, blir den vanligvis lagret i operativsystemets midlertidige filområde. Dette området kan variere avhengig av hvilket operativsystem du bruker. For eksempel på Windows vil filen bli lagret i mappen "C:\Users\brukernavn\AppData\Local\Temp", mens den på Mac kan bli lagret i "/tmp/" mappen.

Det kan også være nyttig å vite at C++ ikke vil slette midlertidige filer automatisk når programmet ditt avsluttes. Det er ditt ansvar å slette filen manuelt ved hjelp av remove() -funksjonen etter at du er ferdig med å bruke den.

## Se også

- [C++ Standard Library - fstream](https://www.cplusplus.com/reference/fstream/)
- [C++ File Handling](https://www.tutorialspoint.com/cplusplus/cpp_files_streams.htm)
- [Creating temporary files in C++](https://www.geeksforgeeks.org/creating-temporary-file-cpp/)