---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:59.772358-07:00
description: "JSON (JavaScript Object Notation) on kevyt muoto datan tallentamiseen\
  \ ja siirt\xE4miseen, mik\xE4 tekee siit\xE4 erinomaisen v\xE4lineen datan vaihtoon\
  \ palvelimien ja\u2026"
lastmod: '2024-03-13T22:44:56.886795-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) on kevyt muoto datan tallentamiseen ja\
  \ siirt\xE4miseen, mik\xE4 tekee siit\xE4 erinomaisen v\xE4lineen datan vaihtoon\
  \ palvelimien ja\u2026"
title: "Ty\xF6skentely JSON:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?

JSON (JavaScript Object Notation) on kevyt muoto datan tallentamiseen ja siirtämiseen, mikä tekee siitä erinomaisen välineen datan vaihtoon palvelimien ja web-sovellusten välillä. Ohjelmoijat käyttävät JSONia sen helpon luettavuuden ihmisten toimesta ja suoraviivaisen jäsentämisen koneiden toimesta, erityisesti työskennellessään sovellusten parissa, jotka vaativat datan vaihtoa internetin yli tai konfiguraatioasetuksia.

## Miten:

C++:ssa ei ole natiivia tukea JSONille, mutta kolmannen osapuolen kirjastot, kuten nlohmann/json, tekevät siitä suoraviivaista. Näin voit käyttää sitä perustehtäviin:

Ensiksi, varmista, että sinulla on kirjasto asennettuna. Jos käytät paketinhallintajärjestelmää, kuten vcpkg tai Conan, voit helposti lisätä `nlohmann/json` projektisi.

### JSONin jäsentäminen merkkijonosta

```cpp
#include <iostream>
#include <nlohmann/json.hpp>

int main() {
    // JSON data merkkijonona
    std::string jsonData = "{\"name\":\"John\", \"age\":30, \"city\":\"New York\"}";

    // Jäsentä JSON merkkijono
    auto jsonObject = nlohmann::json::parse(jsonData);

    // Datan käyttö
    std::cout << "Nimi: " << jsonObject["name"] << "\n"
              << "Ikä: " << jsonObject["age"] << "\n"
              << "Kaupunki: " << jsonObject["city"] << std::endl;

    return 0;
}
```

**Esimerkkitulostus:**

```
Nimi: John
Ikä: 30
Kaupunki: New York
```

### JSONin generointi

JSON datan luominen on yhtä suoraviivaista; sinun tarvitsee vain antaa arvoja `nlohmann/json` objektille.

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // JSON objektin luominen
    nlohmann::json jsonObject;
    jsonObject["name"] = "Jane";
    jsonObject["age"] = 25;
    jsonObject["city"] = "Los Angeles";

    // Muunna JSON objekti merkkijonoksi ja tulosta
    std::string jsonString = jsonObject.dump(4); // Argumentti 4 kauniiseen tulostukseen
    std::cout << jsonString << std::endl;

    return 0;
}
```

**Esimerkkitulostus:**

```
{
    "name": "Jane",
    "age": 25,
    "city": "Los Angeles"
}
```

Nämä esimerkit osoittavat ydintoiminnot työskentelyyn JSONin kanssa C++:ssa käyttäen `nlohmann/json` kirjastoa. Näiden perusteiden avulla voit jäsentää ja luoda JSONia erilaisiin sovelluksiin, konfiguraatiotiedostoista datan vaihtoon verkkosovelluksissa.
