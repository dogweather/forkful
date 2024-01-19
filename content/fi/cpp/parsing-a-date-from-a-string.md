---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän jäsentäminen merkkijonosta tarkoittaa erityispäivän erottamista tekstistä. Ohjelmoijat tekevät tätä yksilöidäkseen ja käsitelläkseen päivämääräkohtaisia ​​tietoja.

## Näin teet:

Käytetään C++ standardikirjaston `<chrono>`-luokkaa, jossa on `from_stream`-metodi:

```C++
#include <chrono>
#include <sstream>
#include <string>

int main() {
    std::string s = "2015-09-15";
    std::istringstream ss(s);

    std::chrono::year_month_day ymd;
    ss >> std::chrono::parse("%F", ymd);

    if(ss.fail()) {
        std::cout << "Parsing failed\n";
    } else {
        std::cout << "Year: " << (int)ymd.year() << ", Month: " 
                  << (unsigned)ymd.month() << ", Day: " 
                  << (unsigned)ymd.day() << "\n";   
    }
    return 0;
}
```

Output:

```code
Year: 2015, Month: 9, Day: 15
```

## Syvä sukellus:

Historiallisesti päivämäärän jäsentäminen on aina ollut haastavaa johtuen eri päivämääräformaatteista. C++20 esitteli `<chrono>`-luokan, jolle annettiin kyky jäsentää päivämääriä, mikä helpottaa tätä prosessia.

Vaihtoehtoisesti voit käyttää Boost.Date_Time -kirjastoa jos vanhan standardin kanssa työskentelet. `from_string(str)` -funktiolla voi jäsentää merkkijonon päivämääräksi:

```C++
#include <boost/date_time/gregorian/gregorian.hpp>

boost::gregorian::date d = boost::gregorian::from_string("2015-09-15");
```

Jäsentämistä voidaan hallinnoida myös manuaalisesti, mutta se vaatii enemmän koodia eikä ole suositeltavaa.

## Katso myös:

- `<chrono>`-kirjasto dokumentaatio: https://en.cppreference.com/w/cpp/chrono
- Boost.Date_Time documentation: https://www.boost.org/doc/libs/1_75_0/doc/html/date_time.html
- `<date>`-kirjasto dokumentaatio: https://github.com/HowardHinnant/date