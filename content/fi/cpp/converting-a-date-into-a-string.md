---
title:                "C++: Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat kohtaavat tarpeen muuttaa päivämäärä merkkijonoksi eri ohjelmointiprojekteissaan. Tässä blogipostissa käymme läpi, miksi tämä on tärkeää ja miten se voidaan tehdä tehokkaasti C++:lla.

## Miten

Konvertointi päivämäärästä merkkijonoksi on helpompaa kuin luulisi. Olemme luoneet yksinkertaisen ohjelman, joka ottaa käyttäjän antaman päivämäärän ja muuttaa sen haluttuun muotoon.

```C++
#include <iostream>
#include <string>
#include <iomanip>

int main() {
    //Kysytään käyttäjältä päivämäärä ja tallennetaan se muuttujaan
    std::string paivamaara;
    std::cout << "Anna päivämäärä muodossa dd.mm.yyyy: ";
    std::cin >> paivamaara;

    //Luodaan output merkkijono ja käytetään put_time-funktiota vaaditun muodon luomiseen
    std::string output;
    std::stringstream ss(paivamaara);
    std::tm t = {};
    ss >> std::get_time(&t, "%d.%m.%Y");
    std::stringstream oss;
    oss << put_time(&t, "%A, %d. %B %Y");
    output = oss.str();

    //Tulostetaan muunnettu päivämäärä halutussa muodossa
    std::cout << "Päivämäärä merkkijonona: " << output << std::endl;
    return 0;
}
```

**Esimerkkitulostus:**

```
Anna päivämäärä muodossa dd.mm.yyyy: 14.07.2021
Päivämäärä merkkijonona: Wednesday, 14. July 2021
```

## Syvemmällä

Vaikka konvertointi päivämäärästä merkkijonoksi näyttää yksinkertaiselta, se voi olla monimutkaisempaa, kun tarvitaan tarkempia muotoja. C++ sisältää kuitenkin hyödyllisiä kirjastoja, kuten `<iomanip>` ja `<ctime>` jotka tekevät tästä prosessista helpompaa. Esimerkiksi `<iomanip>` sisältää put_time-funktion, joka ottaa käyttäjän määrittämän ajan ja muuttaa sen haluttuun muotoon. `<ctime>` sisältää myös hyödyllisiä funktioita, kuten `get_time`, joka muuntaa string-esityksen päivämääräksi.

## Katso myös

- [C++ <iomanip>kirjasto](https://www.cplusplus.com/reference/iomanip/)
- [C++ <ctime>kirjasto](https://www.cplusplus.com/reference/ctime/)