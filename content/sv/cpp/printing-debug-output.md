---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Vad och Varför?

Debug-utskrift är en teknik som hjälper programmerare att diagnostisera och fixera problem i sina program. Genom att utrealisera data på skärmen eller en loggfil kan man mer effektivt spåra och isolera fel.

# Hur man gör:

Att skriva ut debug-utskrift i C++ är ganska enkelt med `cout` i `<iostream>`. Här är ett exempel:

```C++
#include <iostream>

int main() {
  for (int i = 0; i < 5; ++i) {
    std::cout << "Loop iteration: " << i << '\n';
  }
  return 0;
}
```

Utskriften skulle vara:

```
Loop iteration: 0
Loop iteration: 1
Loop iteration: 2
Loop iteration: 3
Loop iteration: 4
```

Vi kan också skriva till en textfil för senare granskning:

```C++
#include <fstream>

int main() {
  std::ofstream fout("debug_log.txt");

  for (int i = 0; i < 5; ++i) {
    fout << "Loop iteration: " << i << '\n';
  }
  fout.close();

  return 0;
}
``` 

Om du öppnar `debug_log.txt`, ser du samma utskrift som ovan.

# Djupdykning

C++ har inkluderat debug-utskrift sedan dess födelse på 1980-talet. Dess syntax har förblivit relativt oförändrad genom åren, vilket är något av en prestation i sig.

Det finns andra sätt att ta del av debug-utskrift. Du kan använda en dedikerad debugger som `gdb` eller använda loggbibliotek som `spdlog` eller `boost::log`.

Vad gäller implementeringsdetaljer är det värt att notera att `cout` och `cerr` från `<iostream>` går till standardutdata respektive standardfel. De skiljer sig åt främst i att `cerr` inte buffrar dess utdata, vilket gör det perfekt för felmeddelanden.

# Se även

Om du vill lära dig mer om debug-utskrift och debugging i allmänhet, kan du kolla in följande resurser:

- ["The Art of Debugging"](https://www.oreilly.com/library/view/the-art-of/9781593271749/)
- [Cppreference - iostream](https://en.cppreference.com/w/cpp/io)
- [SO - How does cerr and cout differ?](https://stackoverflow.com/questions/22157594/difference-between-cout-cerr)
- [Debugging med GDB](https://sourceware.org/gdb/current/onlinedocs/gdb/)