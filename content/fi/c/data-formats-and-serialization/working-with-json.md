---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:57.197941-07:00
description: "Kuinka: JSON:n k\xE4sittelyyn C:ss\xE4 k\xE4ytet\xE4\xE4n tyypillisesti\
  \ kirjastoa kuten `jansson` tai `json-c`, koska C ei tarjoa sis\xE4\xE4nrakennettua\
  \ tukea JSON:lle. T\xE4ss\xE4\u2026"
lastmod: '2024-03-13T22:44:57.064097-06:00'
model: gpt-4-0125-preview
summary: "JSON:n k\xE4sittelyyn C:ss\xE4 k\xE4ytet\xE4\xE4n tyypillisesti kirjastoa\
  \ kuten `jansson` tai `json-c`, koska C ei tarjoa sis\xE4\xE4nrakennettua tukea\
  \ JSON:lle."
title: "Ty\xF6skentely JSONin kanssa"
weight: 38
---

## Kuinka:
JSON:n käsittelyyn C:ssä käytetään tyypillisesti kirjastoa kuten `jansson` tai `json-c`, koska C ei tarjoa sisäänrakennettua tukea JSON:lle. Tässä keskitymme `janssoniin` sen helppokäyttöisyyden ja aktiivisen ylläpidon vuoksi. Asenna ensin kirjasto (esim. käyttäen paketinhallintajärjestelmää kuten `apt` Ubuntussa: `sudo apt-get install libjansson-dev`).

Aloitetaan JSON-merkkijonon jäsentämisestä ja sen sisällön käsittelemisestä:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Nimi: %s\nIkä: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Esimerkkitulo:
```
Nimi: John Doe
Ikä: 30
```

Seuraavaksi, luodaan ja tulostetaan JSON-objekti:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Esimerkkitulo:
```
{"name": "Jane Doe", "age": 25}
```

Nämä esimerkit osoittavat perusteet JSON-merkkijonon lataamisesta, sen arvojen purkamisesta, uuden JSON-objektin luomisesta ja sitten sen tulostamisesta merkkijonona.

## Syväsukellus
Tarve käsitellä JSON:ia C:ssä juontaa juurensa verkon omaksumisesta JSON:ia ensisijaiseksi formaatiksi tietojenvaihdossa. JSON:n yksinkertaisuus ja tehokkuus saivat sen nopeasti syrjäyttämään XML:n, huolimatta C:n alunperin puuttuvasta suorasta tuesta JSON-manipulaatiolle. Varhaiset ratkaisut sisälsivät manuaalista merkkijonojen käsittelyä - virhealtista ja tehotonta. Kirjastot kuten `jansson` ja `json-c` tulivat täyttämään tämän aukon, tarjoten robustit API:t JSON:n jäsentämiseen, rakentamiseen ja sarjoittamiseen.

Vaikka `jansson` tarjoaa yksinkertaisuuden ja helppokäyttöisyyden, `json-c` voi houkutella niitä, jotka etsivät laajempaa ominaisuusjoukkoa. Siitä huolimatta, vaihtoehdot kuten jäsentämiskirjastot C++:ssa tarjoavat monimutkaisempia abstraktioita, kiitos tämän kielen monimutkaisempien tietorakenteiden ja standardikirjaston tuen. Kuitenkin, kun työskennellään ympäristöissä, joissa C on suositeltu tai vaadittu kieli - kuten sulautetuissa järjestelmissä tai käytettäessä olemassa olevia C-kirjastoja - `janssonin` tai `json-c`:n käyttö muuttuu välttämättömäksi.

On myös syytä huomata, että JSON:n käsittely C:ssä edellyttää syvällisempää ymmärtämistä muistinhallinnasta, sillä nämä kirjastot palauttavat usein dynaamisesti varattuja objekteja, jotka vaativat nimenomaista vapauttamista. Tämä haastaa ohjelmoijia tasapainottamaan mukavuutta vastuun kanssa estääkseen muistivuodot, mikä on olennainen osa tehokkaan C-koodin rakentamisessa.
