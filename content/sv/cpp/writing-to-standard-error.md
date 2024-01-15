---
title:                "Skrivande till standardfel"
html_title:           "C++: Skrivande till standardfel"
simple_title:         "Skrivande till standardfel"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error, även känd som stderr, är ett sätt för C++-programmerare att kommunicera felmeddelanden till användaren. Det är ett viktigt verktyg för felsökning och kan hjälpa till att förbättra användarupplevelsen.

## Hur man gör det

För att skriva till stderr behöver du först inkludera standardbiblioteket <iostream> i din kod. Sedan kan du använda funktionen std::cerr för att skriva ditt felmeddelande. Se nedan för ett exempel:

```C++
#include <iostream> // inkludera standardbiblioteket

int main() {
  std::cerr << "Ett fel inträffade!" << std::endl; // skriv till stderr med felmeddelandet
  return 0;
}
```

Detta kommer att skriva ut "Ett fel inträffade!" till stderr och avsluta programmet med en felkod.

## Djupdykning

Det finns flera anledningar till varför det är viktigt att använda stderr för att kommunicera felmeddelanden. För det första är det en standardiserad metod som många andra språk och system använder. Det gör det enklare för användare att förstå och hantera felmeddelanden.

För det andra är det viktigt att använda stderr istället för stdout för att skilja mellan vanlig programoutput och felmeddelanden. Detta kan hjälpa till att undvika förvirring och förbättra läsbarheten för användaren.

För det tredje tillåter stderr dig att hantera olika typer av felmeddelanden separat. Till exempel kan du välja att skicka kritiska felmeddelanden till stderr och mindre allvarliga felmeddelanden till stdout.

## Se också

* [C++ iostream referens](https://www.cplusplus.com/reference/iostream/)
* [Felsökningsguide för C++](https://docs.microsoft.com/sv-se/visualstudio/debugger/navigating-through-code-with-the-debugger?view=vs-2019)