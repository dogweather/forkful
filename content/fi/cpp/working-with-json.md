---
title:                "Työskentely jsonin kanssa"
html_title:           "C++: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Jos olet koskaan ollut tekemisissä datan kanssa, olet todennäköisesti törmännyt JSONiin. Se on yksi yleisimmistä tiedostomuodoista, joka helpottaa datan tallentamista ja välittämistä eri ohjelmistojen välillä. JSON on myös erittäin hyödyllinen silloin, kun tarvitset rakenteellista dataa esimerkiksi verkkosovellusta kehittäessäsi. Nyt kun tiedät miksi JSON on tärkeä, on aika opetella kuinka sitä käytetään.

## Miten

JSONia käytetään C++:ssa `json.hpp` kirjastolla. Ensimmäiseksi sinun tulee ladata tämä kirjasto osoitteesta https://github.com/nlohmann/json/releases ja sisällyttää se projektiisi. Tämän jälkeen voit aloittaa JSONin käytön seuraavien esimerkkien avulla:

```
#include <iostream>
#include "json.hpp"

int main()
{
    // Luo JSON-olio ja lisää siihen dataa
    nlohmann::json data = {
        {"nimi", "Matti Meikäläinen"},
        {"ikä", 35},
        {"työpaikka", "Ohjelmistokehittäjä"}
    };

    // Tulostaa JSONin sisällön
    std::cout << data << std::endl;

    // Voit myös lisätä dataa myöhemmin
    data["sukupuoli"] = "mies";

    // Voit hakea tietyn kentän arvon
    std::string nimi = data["nimi"];

    // Voit myös käydä läpi kaikki kentät
    for (auto it = data.begin(); it != data.end(); ++it) {
        std::cout << it.key() << ": " << it.value() << std::endl;
    }

    // Voit tallentaa JSONin tiedostoon
    std::ofstream file("data.json");
    file << data << std::endl;
    
    return 0;
}
```

Esimerkki JSON-tiedoston sisällöstä tulostettuna:

```
{
  "nimi": "Matti Meikäläinen",
  "ikä": 35,
  "työpaikka": "Ohjelmistokehittäjä"
}
```

## Syväsukellus

JSONilla on monia muita ominaisuuksia, kuten mahdollisuus sisällyttää muita JSON-tiedostoja, tarkistaa onko data olemassa ja poistaa dataa. Voit myös lukea ja kirjoittaa JSONia muilla kielillä, kuten Python ja Java. Tutustu `json.hpp` dokumentaatioon saadaksesi lisätietoja.

## Katso myös

- [JSON-kieliopas](https://www.json.org/json-fi.html)
- [json.hpp dokumentaatio](https://github.com/nlohmann/json)