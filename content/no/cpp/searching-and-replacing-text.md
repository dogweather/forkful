---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:57:27.245107-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Søk og erstatt lar deg finne tekststrenger og skifte dem ut med nye. Programmerere bruker dette for å oppdatere kode, rette feil eller masse-endre data.

## How to: (Slik gjør du det:)
For en enkel C++-oppgave kan du bruke `std::string` og `std::string::replace`. Her er et eksempel:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string text = "Hei, verden! Verden er herlig.";
    std::string text_to_search = "verden";
    std::string replacement = "Norge";

    size_t pos = text.find(text_to_search);

    while (pos != std::string::npos) {
        text.replace(pos, text_to_search.length(), replacement);
        pos = text.find(text_to_search, pos + replacement.length());
    }

    std::cout << text << std::endl; // Skriver ut: Hei, Norge! Norge er herlig.
}
```

## Deep Dive (Dypdykk)
Før moderne C++ hadde vi kanskje brukt C-funksjoner som `strstr` og `strncpy` for tekstmanipulering. C++ tilbyr std::string og algoritmer som `std::replace` og `std::regex_replace` for disse oppgavene. Med `std::regex_replace`, kan du til og med bruke regulære uttrykk for avansert søk og erstatt. Effektiviteten av disse operasjonene kan variere basert på implementasjonen; for eksempel, ved å bruke `find` og `replace` i en løkke, kan det innføre mange allokeringer hvis strengen endres mye.

## See Also (Se også)
- [std::replace](http://en.cppreference.com/w/cpp/algorithm/replace)
- [std::regex_replace](http://en.cppreference.com/w/cpp/regex/regex_replace)
- [C++ Regular Expressions](https://www.cplusplus.com/reference/regex/)
