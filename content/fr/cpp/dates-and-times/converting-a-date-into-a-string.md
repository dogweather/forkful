---
date: 2024-01-20 17:36:21.778918-07:00
description: "How To (Comment faire ?) Avant C++11, le traitement des dates et des\
  \ cha\xEEnes \xE9tait moins standardis\xE9. Avec `<ctime>`, on a une interface h\xE9\
  rit\xE9e du C. Les\u2026"
lastmod: '2024-04-05T22:51:12.076149-06:00'
model: gpt-4-1106-preview
summary: "Avec `<ctime>`, on a une interface h\xE9rit\xE9e du C."
title: "Conversion d'une date en cha\xEEne de caract\xE8res"
weight: 28
---

## How To (Comment faire ?)
```C++
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>

int main() {
    std::time_t t = std::time(nullptr);
    std::tm* tm_ptr = std::localtime(&t);

    std::stringstream ss;
    ss << std::put_time(tm_ptr, "%Y-%m-%d %H:%M:%S");
    std::string date_str = ss.str();

    std::cout << date_str << std::endl; // Output: 2023-04-05 14:55:03 (par exemple)
    return 0;
}
```

## Deep Dive (Plongée en profondeur)
Avant C++11, le traitement des dates et des chaînes était moins standardisé. Avec `<ctime>`, on a une interface héritée du C. Les alternatives comme `boost::date_time` ou les librairies tierces ajoutaient de la flexibilité.

En C++20, la bibliothèque `<chrono>` introduit des outils plus robustes pour la gestion du temps. Cette standardisation facilite les conversions et les manipulations.

Concernant l'implémentation, `std::put_time(std::tm*, const char*)` formate le temps selon l'argument de formatage, où `%Y-%m-%d %H:%M:%S` représente la date et l'heure standards.

## See Also (Voir également)
- [cppreference.com sur `std::put_time`](https://en.cppreference.com/w/cpp/io/manip/put_time)
- [cppreference.com sur `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [Documentation sur `boost::date_time`](https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html)
