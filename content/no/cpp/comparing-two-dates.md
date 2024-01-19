---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

---
## Hva & Hvorfor?
Sammenligne to datoer handler om å bestemme hvilken dato som kommer før eller etter. Dette er nyttig for å sortere hendelser, beregne tidsintervaller, og ved å bestemme utløpsdatoer.

---
## Hvordan gjør man det:
Her er et enkelt eksempel på hvordan sammenligne to datoer i C++ ved hjelp av ```std::chrono``` biblioteket og ```std::time_point```:

```C++
#include <iostream>
#include <chrono>

void sammenligneDatoer(std::chrono::system_clock::time_point dato1, std::chrono::system_clock::time_point dato2) {
    if (dato1 == dato2) {
        std::cout << "Datoene er like." << std::endl;
    } else if (dato1 < dato2) {
        std::cout << "Dato 1 er tidligere enn Dato 2." << std::endl;
    } else {
        std::cout << "Dato 2 er tidligere enn Dato 1." << std::endl;
    }
}

int main() {
    auto dato1 = std::chrono::system_clock::now();
    auto dato2 = std::chrono::system_clock::now() + std::chrono::hours(24);

    sammenligneDatoer(dato1, dato2);

    return 0;
}
```
__
Kjører denne programmet vil resultere i:
"Dato 1 er tidligere enn Dato 2."

---
## Dypdykk:
Historisk sett, ble dato sammenligning ofte gjort manuelt ved bruk av lammende, feilutsatte algoritmer. C++ standardbiblioteket inklusjoner som ```std::chrono``` modulerer kompleksiteten, eliminerer feilmarginen, og gjør dato-sammenligning langt mer intuitivt og mindre utsatt for feil.

Alternativt, kan du bruke biblioteker som Boost for mer komplekse datatidsoperasjoner. Men for enkel dato sammenligning, anbefales ```std::chrono```.

En dypere detalj om ```std::time_point```: Det er en klasse mal i C++ som representer et tidspunkt. En ```std::time_point``` instans har en nulltidsperiode, som er systemklokkes startpunkt (vanligvis er dette Unix Epoch: 01.01.1970).

---
## Se Også:
1. [cppreference :: std::chrono](https://en.cppreference.com/w/cpp/chrono)
2. [cppreference :: std::time_point](https://en.cppreference.com/w/cpp/chrono/time_point)
3. [Boost Library](https://www.boost.org/doc/libs/1_76_0/doc/html/date_time.html)