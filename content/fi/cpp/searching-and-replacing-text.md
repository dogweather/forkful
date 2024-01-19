---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin hakeminen ja korvaaminen on prosessi, jossa tekstiä etsitään spesifin mallin tai merkkijonon perusteella ja korvataan uudella tekstillä, jolla on yleensä eri merkitys tai tarkoitus. Ohjelmoijat tekevät tämän parantaakseen tai päivittääkseen koodiaan tai muuttaakseen dataa.

## Näin se tehdään:

```C++
#include <iostream>
#include <string>

void etsi_ja_korvaa(std::string& lause, const std::string& etsittava, const std::string& korvaava) {
    size_t indeksi = lause.find(etsittava);
    while(indeksi != std::string::npos) {
        lause.replace(indeksi, etsittava.length(), korvaava);
        indeksi = lause.find(etsittava, indeksi + korvaava.length());
    }
}

int main() {
    std::string lause = "Tervetuloa Helsingin ohjelmoijille!";
    etsi_ja_korvaa(lause, "Helsingin", "Tampereen");
    std::cout << lause << std::endl;

    return 0;
}
```

Tämä koodi etsii merkkijonon "Helsingin" ja korvaa sen merkkijonolla "Tampereen". Tulostee: "Tervetuloa Tampereen ohjelmoijille!"

## Syvällisemmin:

Tekstin etsimisen ja korvaamisen historia ulottuu ensimmäisten tekstieditorien ja tulkintaympäristöjen aikoihin. Se on keskeinen osa monen ohjelmoijan työkalupakkia, ja siksi se on standarditoiminto monissa ohjelmointikielissä.

C++:ssa teksin korvaaminen suoritetaan natiivisti `std::string`:in `replace()`-metodilla, joka ottaa korvattavan tekstin indeksit ja korvaavan tekstin. Jos korvattavaa tekstiä ei löydy, `std::string::npos` palautetaan.

On myös olemassa useita tekstin etsimiseen ja korvaamiseen liittyviä kirjastoja, jotka tarjoavat erilaisia työkaluja ja funktioita tätä toimintoa varten, esimerkiksi, `Boost.Regex` ja `std::regex` standardikirjasto.

## Katso myös:

1. C++ reference, `std::string::replace`: https://cplusplus.com/reference/string/string/replace/
2. C++ API-oppaat, `std::string`-luokka: https://cppreference.com/w/cpp/string/basic_string
3. Boost.Regex, säännöllisten lausekkeiden kirjasto: https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html
4. StackOverflow, kysymyksiä ja vastauksia C++:n tekstinkorvauksesta: https://stackoverflow.com/questions/tagged/c%2B%2B+string-replace.