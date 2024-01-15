---
title:                "Läsa en textfil"
html_title:           "C++: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Varför

Att kunna läsa textfiler är en viktig färdighet när man programmerar i C++. Det ger dig möjlighet att använda extern data i ditt program, vilket kan vara avgörande för dess funktionalitet.

## Hur man gör det

För att läsa en textfil i C++ behöver du använda ett filobjekt och en instans av "ifstream"-klassen. Sedan kan du använda "open()" funktionen för att öppna filen och "getline()" funktionen för att läsa varje rad i filen.

```C++
#include <iostream>
#include <fstream>

int main() {
  std::ifstream fil("textfil.txt"); // Skapar ett filobjekt och öppnar filen "textfil.txt"
  std::string rad; // Skapar en variabel för varje rad i textfilen
  
  // While-loop som läser varje rad i filen tills den når slutet
  while (std::getline(fil, rad)) {
    std::cout << rad << std::endl; // Skriver ut varje rad på skärmen
  }
  
  fil.close(); // Stänger filen när läsningen är klar
  return 0;
}
```

Om vi antar att vår textfil "textfil.txt" innehåller följande:

```
Hej,
Det här är en textfil.
```

Så kommer vår kod att skriva ut följande på skärmen:

```
Hej,
Det här är en textfil.
```

## Djupdykning

För att förstå hur filobjekt och "ifstream"-klassen fungerar vid läsning av textfiler, behöver vi förstå begreppet "ström" (stream) i C++. Strömmar är en abstrakt representation av en sekvens av data, de kan vara inriktade (input eller output) och peka på en specifik enhet eller handling (tex en fil). I exemplet ovan är vårt filobjekt "fil" en inriktad input-ström som pekar på vår textfil "textfil.txt".

När vi använder "getline()" funktionen så läser den en hel rad av text från strömmen och sparar det i vår variabel "rad". Sedan flyttar den läsaren till nästa rad i filen och fortsätter tills den når slutet. När filen är klar så stängs strömmen automatiskt när "fil" objektet går ut ur synpunkten.

För att få en djupare förståelse för hur filer och strömmar fungerar i C++, rekommenderas att läsa mer om dem i C++ dokumentationen eller andra resurser på nätet.

## Se även

- [C++ Filsystem](https://www.cplusplus.com/reference/filesystem/)
- [C++ Strömmar](https://www.cplusplus.com/reference/ios/ios/)
- [Textfiler i C++](https://www.geeksforgeeks.org/reading-writing-text-files-c/)