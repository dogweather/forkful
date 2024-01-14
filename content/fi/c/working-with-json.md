---
title:                "C: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi käyttää JSONia?

JSON (JavaScript Object Notation) on yksi suosituimmista tiedostomuodoista, jota käytetään tietojen vaihtamiseen verkossa. Se on yksinkertainen, kevyt ja helppo lukea ja kirjoittaa sekä ihmisille että ohjelmille, mikä tekee siitä erityisen hyödyllisen tietojen tallennukseen ja siirtämiseen. Jos olet C-ohjelmoija, voit hyödyntää JSONia monin eri tavoin, joten jatka lukemista, jotta saat selville kuinka.

## Kuinka käyttää JSONia C-ohjelmoinnissa?

JSON-tiedostojen lukeminen ja kirjoittaminen C-kielellä vaatii muutaman yksinkertaisen askeleen. Ensinnäkin, sinun täytyy sisällyttää `json-c` kirjasto projektiisi. Voit tehdä tämän esimerkiksi käyttämällä `apt-get` tai `yum` pakettienhallintaa riippuen käyttämästäsi Linux-jakelusta.

```
sudo apt-get install libjson-c-dev
```

Kun `json-c` kirjasto on asennettu, voit alkaa käyttämään sitä projektissasi. Alla on yksinkertainen esimerkki, miten voit lukea JSON-tiedoston ja tulostaa sen sisällön konsoliin:

```
#include <stdio.h>
#include <json-c/json.h>

int main() {

    // Avataan JSON-tiedosto
    FILE *file = fopen("tiedosto.json", "r");
    if (!file) {
        printf("Tiedoston avaaminen epäonnistui.\n");
        return 1;
    }

    // Luetaan tiedosto ja tallennetaan JSON-objekti
    char buffer[10000];
    fread(buffer, 1, sizeof(buffer), file);
    struct json_object *jobj = json_tokener_parse(buffer);

    // Suljetaan tiedosto
    fclose(file);

    // Tulostetaan JSON-tiedoston sisältö konsoliin
    printf("Sisältö: %s\n", json_object_to_json_string_ext(jobj, JSON_C_TO_STRING_PRETTY));

    return 0;
}
```

Yllä olevassa esimerkissä käytetään `json_object_to_json_string_ext()` funktiota tulostamaan JSON-objektin sisältö konsoliin. Voit kuitenkin käyttää muita `json-c` kirjaston funktioita, kuten `json_object_object_get()` ja `json_object_array_length()`, käsitelläksesi JSON-objektin sisältöä haluamallasi tavalla.

## Syvä sukellus JSONin kanssa

JSON-objekti koostuu parista, joukosta avain-arvo -pareja. Näitä pareja kutsutaan myös nimellä `json_object` ja `json_object` voi sisältää muita `json_objecteja` sekä `json_arrayeja`. Voit käsitellä ja muokata `json_c` kirjaston funktioiden avulla näitä objekteja haluamallasi tavalla. Suositeltavaa on tutustua tarkemmin `json-c` kirjaston dokumentaatioon löytääksesi kaikki käytettävissä olevat funktiot.

## Katso myös

- <https://json-c.github.io/json-c/>: `json-c` kirjaston virallinen dokumentaatio
- <https://www.geeksforgeeks.org/json-c-example/>: GeeksforGeeks-sivuston artikkeli, jossa esitellään lisää esimerkkejä JSONin käyttämisestä C-ohjelmoinnissa.