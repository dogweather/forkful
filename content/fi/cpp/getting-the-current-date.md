---
title:                "Päivämäärän hakeminen"
html_title:           "C++: Päivämäärän hakeminen"
simple_title:         "Päivämäärän hakeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Koodaajat usein tarvitsevat tietää nykyisen päivämäärän eri ohjelmointiprosesseissa, kuten tiedostojen tallentamisessa, lokiin kirjoittamisessa tai käyttäjäkohtaisissa sovelluksissa.

## Näin teet sen
### Käytä vakiotyökaluja
Yksi helpoimmista tavoista saada nykyinen päivämäärä C++:ssa on käyttää `ctime` kirjastoa. Tämä kirjasto sisältää valmiin funktion `time()`, joka palauttaa nykyisen ajan sekunteina. Voit sitten käyttää `localtime()` funktiota muuttamaan tämän sekuntiarvon paikalliseksi aikarakenteeksi, joka sisältää päivämäärän ja kellonajan.

```C++
#include <iostream>
#include <ctime>

using namespace std;

int main() {
    time_t nykyinen_aika = time(NULL);
    struct tm *paikallinen_aika = localtime(&nykyinen_aika);
    cout << "Päivämäärä: " << paikallinen_aika->tm_mday << "/" << paikallinen_aika->tm_mon+1 << "/" << paikallinen_aika->tm_year+1900 << endl;
    return 0;
}
```

### Käytä C++11:ssä lisättyä `<chrono>` kirjastoa
C++11 toi mukanaan uuden aikakirjaston, `<chrono>`, joka mahdollistaa entistä tarkemman ajan hallinnan. Voit käyttää sitä seuraavasti saadaksesi nykyisen päivämäärän:

```C++
#include <iostream>
#include <chrono>

using namespace std;
using namespace std::chrono;

int main() {
    system_clock::time_point nykyinen_aika = system_clock::now();
    time_t nykyinen_aika_aikaleima = system_clock::to_time_t(nykyinen_aika);
    cout << "Päivämäärä: " << ctime(&nykyinen_aika_aikaleima) << endl;
    return 0;
}
```

Huomaat ehkä, että tämä lähestymistapa antaa enemmän tietoa, kuten kellonajan.

### Käytä kirjastoa `<ctime>` tarkemmin
Joissakin tilanteissa saatat tarvita vielä tarkempia tietoja nykyisestä päivämäärästä, kuten millisekunnit tai mikrosekunnit. Voit tehdä tämän käyttämällä `<ctime>` kirjaston lisää funktioita, kuten `clock()` tai `gettimeofday()`. Nämä antavat lisätietoja ohjelman suoritusajasta, ja voit käyttää niitä laskemaan nykyisen päivämäärän tarkemmin.

## Syvällisempi tarkastelu
Nykyisen päivämäärän saaminen on toiminto, joka sisältää monia erilaisia lähestymistapoja ja vaihtoehtoja C++:ssa. Voit lukea lisää eri kirjastoista ja funktioista, jotka auttavat sinua saamaan haluamasi tiedot nykyisestä päivämäärästä.

## Katso myös
- [C++ time library reference](https://en.cppreference.com/w/cpp/chrono)
- [C library ctime reference](https://en.cppreference.com/w/cpp/chrono)
- [C++ strftime function reference](https://en.cppreference.com/w/cpp/chrono/c/strftime)