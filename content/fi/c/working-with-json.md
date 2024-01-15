---
title:                "Työskentele jsonin kanssa"
html_title:           "C: Työskentele jsonin kanssa"
simple_title:         "Työskentele jsonin kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

JSON (JavaScript Object Notation) on yksi yleisimmistä tietojen siirtomuodoista ohjelmoinnissa. Se on erittäin kätevä tapa siirtää ja tallentaa erilaisia tietotyyppejä, kuten numeroita, merkkijonoja ja objekteja, ja sen käyttö on yleistynyt erityisesti web-sovelluksissa. Tässä artikkelissa käymme läpi, kuinka voit työskennellä JSONin kanssa C-ohjelmointikielellä ja kerromme, miksi se kannattaa.

## Kuinka

Alla olevassa esimerkissä näytämme, kuinka voit luoda JSON-muotoisen rakenteen C-koodilla ja tulostaa sen konsoliin käyttämällä `printf()`-funktiota:

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main(){
  //luo JSON-muotoinen objecti
  json_object *jobj = json_object_new_object();
  //lisää numeroarvo objectiin
  json_object_object_add(jobj, "luku", json_object_new_int(42));
  //lisää merkkijono objectiin
  json_object_object_add(jobj, "teksti", json_object_new_string("Hei maailma!"));
  //tulosta objecti konsoliin
  printf("JSON-tulostus: %s\n", json_object_to_json_string(jobj));
  return 0;
}
```

Koodin suorituksen jälkeen tulostuu seuraava tulos:

```
JSON-tulostus: {"luku":42, "teksti":"Hei maailma!"}
```

Voit myös lukea JSON-tiedostoja ja käsitellä niiden sisältöä C-koodilla käyttäen `json-c` -kirjastoa. Alla olevassa esimerkissä luemme tiedostosta JSON-muotoisen datan ja tulostamme sen konsoliin:

```C
#include <stdio.h>
#include <stdlib.h>
#include <json-c/json.h>

int main(){
  //määritä tiedostonimi
  char *filename = "tiedosto.json";
  //avataan tiedosto lukua varten
  FILE *file = fopen(filename, "r");
  //lue tiedostosta JSON-data
  char *json_data = (char*) malloc(sizeof(char) * 100);
  fread(json_data, 1, 100, file);
  //muunna data JSON-objectiksi
  json_object *jobj = json_tokener_parse(json_data);
  //tulosta objectin sisältö konsoliin
  printf("JSON-tulostus: %s\n", json_object_to_json_string(jobj));
  return 0;
}
```

Jos tiedoston sisältö on esimerkiksi seuraava:

```
{"nimi": "Matti", "ika": 30}
```

Tulostuu seuraava:

```
JSON-tulostus: {"nimi": "Matti", "ika": 30}
```

## Syväsukellus

JSONin käyttö C-koodissa on mahdollista käyttämällä `json-c` -kirjastoa, joka sisältää monia hyödyllisiä funktioita JSON-datan käsittelyyn. Kirjastoon pääset tutustumaan tarkemmin esimerkiksi täältä: [https://github.com/json-c/json-c](https://github.com/json-c/json-c).

Tärkein asia, joka kannattaa huomioida käytettäessä JSONia C-koodissa, on varmistaa, että JSON-datan sisältö vastaa odotettua. Esimerkiksi jos odotamme JSON-datan sisältävän tiettyjä avaimia ja arvoja, niitä kannattaa tarkastella käyttäen `json_object_object_get()` -funktiota.

```
json_object *arvo = json_object_object_get(jobj, "avain");
if(arvo == NULL) {
  //arvoa ei löytynyt
} else {
  //arvo löytyi
}
```

## Katso myös

-