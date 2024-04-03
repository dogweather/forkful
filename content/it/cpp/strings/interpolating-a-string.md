---
date: 2024-01-20 17:50:30.694068-07:00
description: 'How to: C++ moderno offre diverse strade per interpolare stringhe. Vediamone
  un paio: Con `std::format` (C++20 in poi).'
lastmod: '2024-03-13T22:44:43.712263-06:00'
model: gpt-4-1106-preview
summary: C++ moderno offre diverse strade per interpolare stringhe.
title: Interpolazione di una stringa
weight: 8
---

## How to:
C++ moderno offre diverse strade per interpolare stringhe. Vediamone un paio:

Con `std::format` (C++20 in poi):
```C++
#include <format>
#include <iostream>
#include <string>

int main() {
    std::string name = "Mondo";
    int year = 2023;
    std::string greeting = std::format("Ciao, {}! Siamo nell'anno {}.", name, year);
    std::cout << greeting << std::endl;
    return 0;
}
```
Output:
```
Ciao, Mondo! Siamo nell'anno 2023.
```

Con `ostringstream`:
```C++
#include <sstream>
#include <iostream>
#include <string>

int main() {
    std::ostringstream oss;
    std::string name = "Mondo";
    int year = 2023;
    oss << "Ciao, " << name << "! Siamo nell'anno " << year << ".";
    std::string greeting = oss.str();
    std::cout << greeting << std::endl;
    return 0;
}
```
Output:
```
Ciao, Mondo! Siamo nell'anno 2023.
```

## Deep Dive:
Prima di C++20, i programmatori spesso usavano `sprintf` o concatenazione manuale, che era complicata e incline ad errori. `std::format` semplifica enormemente interpolazione, con una sintassi chiara e flessibilità.

**Alternative:**
- `boost::format`: se non hai C++20, Boost fornisce una soluzione simile a `std::format`.
- `fmtlib/fmt`: una libreria che offre capacità di formattazione prima che `std::format` fosse aggiunto allo standard.

**Dettagli implementativi:**
`std::format` usa un approccio "type-safe" e previene molti degli errori comuni che si verificavano con `sprintf`. Questo metodo permette anche di formattare tipi definiti dall'utente (UDT) tramite la personalizzazione delle specializzazioni di `std::formatter`.

## See Also:
- [La documentazione di `std::format`](https://en.cppreference.com/w/cpp/utility/format)
- [fmtlib ('fmt') GitHub Repository](https://github.com/fmtlib/fmt)
- [Boost.Format documentation](https://www.boost.org/doc/libs/1_75_0/libs/format/)
