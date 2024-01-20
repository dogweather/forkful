---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? – Mikä & Miksi?
Standardivirhe on toinen tekstivirta ohjelmalle. Kirjoitamme siihen virheviestit, koska erottaa ne tavallisesta tulosteesta, jolloin ongelmat on helpompi bongata.

## How to: – Miten:
```C++
#include <iostream>

int main() {
    std::cerr << "Error: File not found" << std::endl;
    // Normaali tulostus pysyy erillään.
    std::cout << "Ohjelma jatkuu..." << std::endl;
    return 0;
}
```

### Esimerkkituloste:
```
Error: File not found
Ohjelma jatkuu...
```

## Deep Dive – Syväsukellus:
Historiallisesti standardivirhe erotettiin, jotta virheviestit voidaan käsitellä eri tavalla. Vaihtoehtoja kuten tiedostoon kirjoitus löytyy, mutta `std::cerr` on yleinen tapa virheiden käsittelyyn C++:ssa. `std::cerr` käyttää puskuroimatonta tulostusta, joka tarkoittaa, että viestit ilmestyvät heti eikä odota puskurin täyttymistä.

## See Also – Katso Myös:
- C++ Standard Library reference: https://en.cppreference.com/w/cpp/io/cerr
- C++ error handling techniques: https://www.cplusplus.com/doc/tutorial/exceptions/
- Understanding C++ streams: https://www.learncpp.com/cpp-tutorial/input-and-output-io-streams/