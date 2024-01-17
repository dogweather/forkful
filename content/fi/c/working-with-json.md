---
title:                "Työskentely jsonin kanssa"
html_title:           "C: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-json.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

JSON (JavaScript Object Notation) on yksinkertainen ja kevyt tapa tallentaa ja jakaa tietoa eri sovellusten välillä. Se on tekstimuotoinen, mikä tekee siitä helpon lukea ja kirjoittaa. JSONia käytetään usein verkkosovelluksissa tiedon siirtämiseen, kuten API-kutsuissa.

## Miten:

Hyödyllinen tapa käsitellä JSONia C-ohjelmoinnissa on käyttää JSON-C -kirjastoa. Kirjasto tarjoaa joukon toimintoja, joilla voit muuntaa JSON-muotoista dataa C:n sisäiseen tietorakenteeseen ja takaisin. Alla on yksinkertainen esimerkki JSON-tiedoston lukemisesta ja tulostamisesta C-koodilla.

```C
#include <stdio.h>
#include <json-c/json.h>

int main()
{
    struct json_object *myobj;
    struct json_object *field;
    myobj = json_object_from_file("data.json"); // avaa JSON-tiedosto
    field = json_object_object_get(myobj, "name"); // muuta "nimi" -kenttä C:n tietotyyppiin
    printf("Tervetuloa %s!\n", json_object_get_string(field)); // tulosta "nimi" -kentän arvo
    json_object_put(myobj); // vapauta muisti
    return 0;
}
```

Tulos tulostetaan seuraavasti, kun tiedoston JSON-tiedosto sisältää kentän "name" arvon "Matti":

```
Tervetuloa Matti!
```

## Syvemmälle:

JSON kehitettiin alunperin käytettäväksi JavaScript-sovelluksissa, mutta nykyään sitä käytetään monissa muissakin ohjelmointikielissä, kuten C:ssä. On myös olemassa muita vaihtoehtoisia kirjastoja, kuten yleisesti käytetty cJSON.

JSON-muoto on helposti luettavissa ja muokattavissa ihmisille, ja se mahdollistaa monimutkaisenkin datan tallentamisen yksinkertaisella rakenteella. JSON-tiedosto sisältää objekteja, joilla on avain-arvo -pareja. Esimerkiksi "name": "Matti" tarkoittaa, että objektilla on avain "name" ja sen arvo on "Matti".

## Katso myös:

- [JSON-C](https://github.com/json-c/json-c) - Kirjasto JSONin käsittelyyn C-ohjelmoinnissa.
- [cJSON](https://github.com/DaveGamble/cJSON) - Yleinen JSON-kirjasto C-ohjelmointiin.
- [JSON.org](https://www.json.org) - JSON:in virallinen verkkosivusto.