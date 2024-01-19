---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor? 

I programmering refererer søk og erstatt til operasjonen hvor vi finner en bestemt tekststreng (søk) og erstatter den med en annen (erstatt). Denne teknikken er svært nyttig for å manipulere data, rette feil og optimalisere kode.

## Slik gjør du:

Her er et eksempel på hvordan du kan søke etter og erstatte tekst i C++:

```C++
#include <string>

int main() {
    std::string tekst = "Hei, mitt navn er Ole";
    std::size_t pos = tekst.find("Ole");

    if(pos != std::string::npos)
    {
        tekst.replace(pos, 3, "Kari");
    }

    std::cout << tekst << std::endl;

    return 0;
}
```

Når du kjører koden over, vil output være:

```
Hei, mitt navn er Kari
```

## Dypdykk

*Søk og erstatte* ble først populært som en funksjon i tekstbehandlingsprogrammer, før det ble tatt i bruk i programmering. I C++, bruker vi ofte `std::string::find` metoden for å søke, og `std::string::replace` metoden for å erstatte tekst.

Alternativt kan man også bruke funksjoner som `std::regex_replace` for mer komplekse søk og erstatningsoperasjoner. Implementeringsdetaljene ligger i C++ Standard Library, spesielt i `<string>` og `<regex>` header-filene.

## Se også

For mer informasjon, se:
- [std::string::find dokumentasjon](http://www.cplusplus.com/reference/string/string/find/)
- [std::string::replace dokumentasjon](http://www.cplusplus.com/reference/string/string/replace/)
- [std::regex_replace dokumentasjon](http://www.cplusplus.com/reference/regex/regex_replace/)