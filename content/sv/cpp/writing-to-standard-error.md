---
title:                "Skriva till standardfel"
html_title:           "C++: Skriva till standardfel"
simple_title:         "Skriva till standardfel"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva till standard error är en vanlig praxis bland programmerare för att skicka felmeddelanden och annan viktig information till standard error-strömmen, istället för att skriva till standard output som kan vara upptagen med annan användardata.

## Så här gör du:
Du kan skriva till standard error i C++ genom att använda funktionen ```std::cerr```, som finns tillgänglig i standardbiblioteket ```<iostream>```. Här är ett exempel på hur man skriver en felmeddelande till standard error:

```
#include <iostream>
using namespace std;

int main() {
  cerr << "Detta är ett felmeddelande!" << endl;
  return 0;
}
```

Kör detta program kommer att skriva ut "Detta är ett felmeddelande!" till standard error-strömmen.

## Djupdykning:
Att skriva till standard error har varit en standardpraxis sedan tidigt skede av programmering, då det var det enda sättet att skicka felmeddelanden. Men numera finns det flera alternativ som ger mer kontroll och flexibilitet, som till exempel loggning med hjälp av bibliotek som Boost Log och spdlog. 

För att optimera prestanda och undvika onödig påverkan på standard output-strömmen, implementeras standard error ibland som en separat ström som är kopplad till en annan fil eller enhet.

## Se även:
- https://en.cppreference.com/w/cpp/io/cerr
- https://www.boost.org/doc/libs/1_58_0/libs/log/doc/html/index.html
- https://github.com/gabime/spdlog