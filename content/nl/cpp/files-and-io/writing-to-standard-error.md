---
title:                "Schrijven naar standaardfout"
aliases:
- /nl/cpp/writing-to-standard-error.md
date:                  2024-01-28T22:13:29.063582-07:00
model:                 gpt-4-0125-preview
simple_title:         "Schrijven naar standaardfout"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/cpp/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Schrijven naar standaardfout (`stderr`) betekent het verzenden van foutmeldingen en diagnostiek naar een aparte stroom, los van reguliere uitvoer (`stdout`). Programmeurs doen dit voor een duidelijke scheiding van normale uitvoer en fouten, waardoor programmatuuruitvoer makkelijker te hanteren en debuggen is.

## Hoe:
C++ gebruikt `cerr` voor het schrijven naar `stderr`. Hier is hoe je het gebruikt:

```cpp
#include <iostream>

int main() {
    std::cout << "Dit is reguliere uitvoer" << std::endl;
    std::cerr << "Dit is een foutmelding" << std::endl;
    return 0;
}
```

Een voorbeelduitvoer zou er zo uitzien:

```
Dit is reguliere uitvoer
Dit is een foutmelding
```

Zelfs als je `stdout` omleidt, blijft `stderr` zichtbaar in de terminal:

```cpp
// Leidt stdout om naar een bestand, maar stderr blijft zichtbaar in de terminal
int main() {
    freopen("uitvoer.txt", "w", stdout);
    std::cout << "Dit wordt niet weergegeven in de terminal" << std::endl;
    std::cerr << "Dit wordt wel weergegeven in de terminal" << std::endl;
    fclose(stdout);
    return 0;
}
```

## Diepere Duik:
In UNIX-achtige systemen is `stderr` geïntroduceerd om programmuitvoer (`stdout`) te scheiden van foutmeldingen, elk met zijn eigen bestandsdescriptor (1 voor `stdout`, 2 voor `stderr`). Alternatieven voor `cerr` zijn het gebruik van `fprintf(stderr, ...)` in C of rechtstreeks schrijven naar bestandsdescriptor 2. Intern is `cerr` een instantie van `ostream` en is ongebufferd om directe foutuitvoer te garanderen zonder te wachten tot de buffer vol is.

## Zie Ook:
- [cppreference std::cerr](https://en.cppreference.com/w/cpp/io/cerr)
- [GNU C Bibliotheek: Standaardstromen](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- [Omleiden van stdout en stderr](http://www.cplusplus.com/reference/cstdio/freopen/)
