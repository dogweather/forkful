---
aliases:
- /no/cpp/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:34.426413-07:00
description: "\xC5 tolke en dato fra en streng inneb\xE6rer \xE5 forst\xE5 strengformatet\
  \ for \xE5 trekke ut dato-komponenter som dag, m\xE5ned og \xE5r. Programmerere\
  \ gj\xF8r dette for \xE5\u2026"
lastmod: 2024-02-18 23:08:54.207584
model: gpt-4-0125-preview
summary: "\xC5 tolke en dato fra en streng inneb\xE6rer \xE5 forst\xE5 strengformatet\
  \ for \xE5 trekke ut dato-komponenter som dag, m\xE5ned og \xE5r. Programmerere\
  \ gj\xF8r dette for \xE5\u2026"
title: Analysering av en dato fra en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å tolke en dato fra en streng innebærer å forstå strengformatet for å trekke ut dato-komponenter som dag, måned og år. Programmerere gjør dette for å håndtere brukerinndata, lese datafiler eller samhandle med API-er som kommuniserer datoer i strengformater. Det er essensielt for databehandling, validering, og utføring av datoaritmetikk i applikasjoner.

## Hvordan gjøre det:
I moderne C++, kan du bruke biblioteket `<chrono>` for å håndtere datoer og tider på en nativ måte, men det støtter ikke direkte tolking fra strenger uten manuell parsing for mer komplekse formater. Imidlertid, for ISO 8601 datoformater og enkle tilpassede formater, her er hvordan du kan oppnå parsing.

**Bruk av `<chrono>` og `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 format
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Tolket dato: " << parsed_date << std::endl;
    } else {
        std::cout << "Klarte ikke å tolke dato." << std::endl;
    }
    
    return 0;
}
```
Eksempel på utdata:
```
Tolket dato: 2023-04-15
```

For mer komplekse formater, eller når man har å gjøre med eldre C++ versjoner, er tredjepartsbiblioteker som `date.h` (Howard Hinnant's datobibliotek) populære. Her er hvordan du kan parse forskjellige formater med det:

**Bruk av `date.h` biblioteket:**
Sørg for at du har biblioteket installert. Du kan finne det [her](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "April 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Tolket dato: " << parsed_date << std::endl;
    } else {
        std::cout << "Klarte ikke å tolke dato fra streng." << std::endl;
    }

    return 0;
}
```
Eksempel på utdata (kan variere avhengig av systemets lokale innstillinger og datooppsett):
```
Tolket dato: 2023-04-15
```
