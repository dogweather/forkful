---
title:                "C++: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kolla om en mapp finns är en viktig del av många program som arbetar med filhantering. Det kan hjälpa till att undvika felaktig hantering av filer och också underlätta för användaren genom att indikera om en viss mapp finns tillgänglig för användning.

## Så här gör du

```C++
#include <iostream>
#include <filesystem>

namespace fs = std::filesystem;

int main() {

   // Ange sökvägen till mappen du vill kontrollera
   std::string path = "/dokument/testmapp/";

   // Använd exists() för att kontrollera om mappen finns
   if (fs::exists(path)) {
      std::cout << "Mappen finns." << std::endl;
   } else {
      std::cout << "Mappen finns inte." << std::endl;
   }

   return 0;
}
```

### Exempeloutput:

Mappen finns inte.

I det här exemplet används C++ standardbiblioteket "filesystem" för att enkelt kontrollera om en given sökväg representerar en befintlig mapp. Det är enkelt att implementera och ger snabbt resultat.

## Djupdykning

Att kontrollera om en mapp faktiskt finns kan kräva lite mer förståelse av hur ett operativsystem hanterar filer och mappar. I sin enklaste form innebär detta att gå igenom alla filsystemsnivåer som hierarkin i en mappstruktur består av.

I vissa fall kan det också vara användbart att känna till och kunna hantera eventuella fel och undantag som kan uppstå vid kontrollen av en mapp. Till exempel kan en otillräcklig behörighet förhindra att en mapp kontrolleras korrekt.

## Se även

- [C++ standardbiblioteket "filesystem"](https://en.cppreference.com/w/cpp/filesystem)
- [Kontrollera behörighet i C++](https://www.geeksforgeeks.org/directory-permissions-in-c-programming-language/)