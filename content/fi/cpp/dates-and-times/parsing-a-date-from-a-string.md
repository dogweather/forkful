---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:46.484061-07:00
description: "Miten: Nykyisess\xE4 C++:ssa voit k\xE4ytt\xE4\xE4 `<chrono>`-kirjastoa\
  \ k\xE4sitell\xE4ksesi p\xE4iv\xE4m\xE4\xE4ri\xE4 ja aikoja natiivisti, mutta se\
  \ ei suoraan tue j\xE4sennyksi\xE4\u2026"
lastmod: '2024-03-13T22:44:56.874515-06:00'
model: gpt-4-0125-preview
summary: "Nykyisess\xE4 C++:ssa voit k\xE4ytt\xE4\xE4 `<chrono>`-kirjastoa k\xE4sitell\xE4\
  ksesi p\xE4iv\xE4m\xE4\xE4ri\xE4 ja aikoja natiivisti, mutta se ei suoraan tue j\xE4\
  sennyksi\xE4 merkkijonoista ilman manuaalista j\xE4sennyst\xE4 monimutkaisemmissa\
  \ muodoissa."
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
weight: 30
---

## Miten:
Nykyisessä C++:ssa voit käyttää `<chrono>`-kirjastoa käsitelläksesi päivämääriä ja aikoja natiivisti, mutta se ei suoraan tue jäsennyksiä merkkijonoista ilman manuaalista jäsennystä monimutkaisemmissa muodoissa. Kuitenkin ISO 8601 -päivämäärämuotoille ja yksinkertaisille mukautetuille muodoille, tässä on miten voit saavuttaa jäsennyksen.

**Käyttäen `<chrono>` ja `<sstream>`:**
```cpp
#include <iostream>
#include <sstream>
#include <chrono>
#include <iomanip>

int main() {
    std::string date_str = "2023-04-15"; // ISO 8601 -muoto
    std::istringstream iss(date_str);
    
    std::chrono::year_month_day parsed_date;
    iss >> std::chrono::parse("%F", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Jäsennetty päivämäärä: " << parsed_date << std::endl;
    } else {
        std::cout << "Päivämäärän jäsentäminen epäonnistui." << std::endl;
    }
    
    return 0;
}
```
Esimerkkitulo:
```
Jäsennetty päivämäärä: 2023-04-15
```

Monimutkaisemmissa muodoissa tai käytettäessä vanhempia C++ -versioita, suositut kolmannen osapuolen kirjastot kuten `date.h` (Howard Hinnantin päivämääräkirjasto) ovat suosittuja. Tässä on miten voit jäsentää erilaisia muotoja sen avulla:

**Käyttäen `date.h` Kirjastoa:**
Varmista, että sinulla on kirjasto asennettuna. Voit löytää sen [täältä](https://github.com/HowardHinnant/date).

```cpp
#include "date/date.h"
#include <iostream>

int main() {
    std::string date_str = "Huhtikuu 15, 2023";
    
    std::istringstream iss(date_str);
    date::sys_days parsed_date;
    iss >> date::parse("%B %d, %Y", parsed_date);
    
    if (!iss.fail()) {
        std::cout << "Jäsennetty päivämäärä: " << parsed_date << std::endl;
    } else {
        std::cout << "Merkkijonosta päivämäärän jäsentäminen epäonnistui." << std::endl;
    }

    return 0;
}
```
Esimerkkitulo (voi vaihdella järjestelmäsi paikallisten asetusten ja päivämääräasetusten mukaan):
```
Jäsennetty päivämäärä: 2023-04-15
```
