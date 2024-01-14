---
title:                "C++: Päivämäärän hankkiminen"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Monet C++-kehitteäjät tarvitsevat ohjelmiensa toiminnalle ajankohtaista päivämäärä- ja aikatietoa. Tämä on tärkeää esimerkiksi tiedostoja tallennettaessa tai lokitietojen kirjaamisessa. Tässä blogitekstissä opit, kuinka voit helposti saada tietokoneesi nykyisen päivämäärän C++:lla.

## Miten

On olemassa monia tapoja saada käyttöjärjestelmälle nykyinen päivämäärä C++:lla. Yksi tapa on käyttää <ctime> -kirjastoa ja sen funktiota time(). Se palauttaa arvon, joka edustaa sekunteja nykyhetken ja 1. tammikuuta 1970 välimaastossa. Tämän avulla voimme käyttää struct tm:ää ja sen avulla saada tarvittavat tiedot päivästä, kuukaudesta ja vuodesta.

```C++
#include <iostream>
#include <ctime>
using namespace std;

int main()
{
    time_t now = time(0);   // Gets the current time

    tm *ltm = localtime(&now);  // Convert the time to local time

    // Print current date and time
    cout << "Current date: " << ltm->tm_mday << "/"<< ltm->tm_mon + 1 << "/" << ltm->tm_year + 1900;
    cout << "Current time: " << ltm->tm_hour << ":" << ltm->tm_min << ":" << ltm->tm_sec << endl;

    return 0;
}
```

Output:

```
Current date: 23/8/2020
Current time: 16:38:52
```

## Syvällinen sukellus

<ctime> -kirjasto tarjoaa myös muita käteviä funktioita päivämäärän ja ajan käsittelyyn, kuten mktime(), joka muuntaa struct tm:n takaisin time_t-muotoon, sekä strftime(), joka mahdollistaa päivämäärän ja ajan tulostamisen haluamassasi muodossa käyttäen formaattivaihtoehtoja.

On myös mahdollista käyttää C++11:sta lähtien mahdollistettua std::chrono -kirjastoa, joka tarjoaa modernin tavan käsitellä päivämääriä ja aikoja. Tämä kirjasto tarjoaa valmiita luokkia, kuten std::chrono::system_clock ja std::chrono::time_point, jotka helpottavat päivämäärä- ja aikatietojen käsittelyä.

## Katso myös

- C++ ctime library reference: https://www.cplusplus.com/reference/ctime/
- std::chrono library reference: https://en.cppreference.com/w/cpp/chrono
- Date and Time in C++: https://www.geeksforgeeks.org/date-time-programming-c/