---
title:    "C++: ”Nykyisen päivämäärän saaminen”"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Miksi

Monissa C++ ohjelmissa on tarve saada käyttöön nykyinen päivämäärä. Tämä voi olla tarpeellista esimerkiksi laskutoimitusten yhteydessä, kun halutaan laskea esimerkiksi ikä tai vertailla päivämääriä. Tässä blogikirjoituksessa käymme läpi, miten nykyinen päivämäärä saadaan ohjelman käyttöön.

## Miten

Päivämäärän saamiseen C++ ohjelmassa käytetään standardikirjaston <chrono> ja <ctime> ohjelmakirjastoja. Näiden avulla ohjelma pystyy käsittelemään erilaisia aika- ja päivämäärätietoja.

Alla näet esimerkin, miten saat nykyisen päivämäärän tulostettua konsoliin:

```C++
// tarvittavien kirjastojen tuonti
#include <chrono>
#include <ctime>
#include <iostream>

// vakiot ajan ja päivämäärän esittämiseen
const int SECONDS_IN_DAY = 86400;

// päivämääränmuokkausohjelman esittely
std::tm* getParsedDate(std::time_t time) {
  std::tm* parsedTime = std::localtime(&time); // saadaan käyttöpaikkaan sidottu aika
  return parsedTime;
}

int main() {
  // haetaan nykyinen aika ja päivämäärä
  std::time_t now = std::time(0);
  // muokataan päivämäärämuotoon halutun tiedon saamiseksi
  std::tm* currentDate = getParsedDate(now);
  // haetaan vuosi, kuukausi ja päivä erikseen
  int year = currentDate->tm_year + 1900;
  int month = currentDate->tm_mon + 1;
  int day = currentDate->tm_mday;
  
  // tulostetaan päivämäärä konsoliin
  std::cout << day << "." << month << "." << year << std::endl;
  
  return 0;
}

```

Esimerkkituloste:
```
11.7.2021
```

## Syventävä tarkastelu

Edellisessä esimerkissä käytettiin std::time ja std::localtime funktioita saamaan nykyinen päivämäärä ja muuntamaan se haluttuun muotoon. On myös mahdollista käyttää muita funktioita, kuten std::chrono::system_clock ja std::chrono::steady_clock. Nämä tarjoavat erilaisia aikamuotoja ja toimivat hieman eri tavalla kuin aiemmat esimerkkifunktiot.

On myös hyvä huomata, että aikaa ja päivämäärää käsitellessä kannattaa ottaa huomioon eri aikavyöhykkeiden vaikutukset. Esimerkiksi std::time muodostaa ajan UTC-aikavyöhykkeen mukaan, kun taas std::localtime käyttää käyttöpaikkaan sidottua aikaa.

## Katso myös

- [std::chrono ohjelmakirjasto](https://en.cppreference.com/w/cpp/chrono)
- [std::ctime ohjelmakirjasto](https://en.cppreference.com/w/cpp/chrono/c/ctime)
- [std::localtime funktio](https://en.cppreference.com/w/cpp/chrono/localtime)
- [std::chrono::system_clock funktio](https://en.cppreference.com/w/cpp/chrono/system_clock)
- [std::chrono::steady_clock funktio](https://en.cppreference.com/w/cpp/chrono/steady_clock)