---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:46.484061-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta tarkoittaa merkkijonon\
  \ muodon tulkintaa p\xE4iv\xE4m\xE4\xE4r\xE4n osien, kuten p\xE4iv\xE4n, kuukauden\
  \ ja vuoden, erottamiseksi.\u2026"
lastmod: '2024-03-13T22:44:56.874515-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta tarkoittaa merkkijonon\
  \ muodon tulkintaa p\xE4iv\xE4m\xE4\xE4r\xE4n osien, kuten p\xE4iv\xE4n, kuukauden\
  \ ja vuoden, erottamiseksi.\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n j\xE4sennys merkkijonosta"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Päivämäärän jäsennys merkkijonosta tarkoittaa merkkijonon muodon tulkintaa päivämäärän osien, kuten päivän, kuukauden ja vuoden, erottamiseksi. Ohjelmoijat tekevät tämän käsitelläkseen käyttäjän syötettä, lukeakseen data-tiedostoja tai ollakseen vuorovaikutuksessa API:en kanssa, jotka välittävät päivämääriä merkkijonomuodossa. Se on olennaista datan käsittelyä, validointia ja päivämäärien aritmetiikan suorittamista varten sovelluksissa.

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
