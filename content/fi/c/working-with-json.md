---
title:                "JSON-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Arduino: JSON-tiedostojen käsittely"
simple_title:         "JSON-tiedostojen käsittely"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
JSON, eli JavaScript Object Notation, on kevyt dataformaatti tiedon vaihtoon. Ohjelmoijat käyttävät JSONia, koska se on yksinkertainen, ihmisen luettava ja se integroituu saumattomasti useimpiin ohjelmointikieliin, kuten C:hen.

## How to: (Miten tehdään:)
C-kielisessä ohjelmoinnissa JSON-dataa käsitellään kirjastoilla. Yleinen kirjasto on `cJSON`. Aloitetaan cJSON:n asennus ja JSONin luonti.

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
    // Luo uusi JSON-objekti
    cJSON *user = cJSON_CreateObject();
    
    if(user == NULL) {
        goto end;
    }
    
    // Lisää data JSON-objektiin
    cJSON_AddNumberToObject(user, "id", 1234);
    cJSON_AddStringToObject(user, "name", "Mikko");

    // Tulosta JSON merkkijonona
    char *user_string = cJSON_Print(user);
    if(user_string == NULL) {
        goto end;
    }
    
    printf("%s\n", user_string);
    
    // Vapauta muisti
    end:
    cJSON_Delete(user);
    free(user_string);
    
    return 0;
}
```

Kun ajat ohjelmaa, tuloste näyttää tältä:

```plaintext
{
    "id":1234,
    "name":"Mikko"
}
```

## Deep Dive (Syväluotaus)
JSON syntyi 2000-luvun alussa, ja siitä tuli nopeasti suosittu XML:n hankaluuden rinnalla. C-kielen kirjastoja, kuten cJSON ja Jansson, käytetään JSON-tiedon käsittelyyn C-ohjelmissa. Kirjastojen sisäiset toiminnot käyttävät usein mallia, missä muistinhallinta on ohjelmoijan vastuulla. Käytettäessä näitä kirjastoja, varmista, että vapautat kaiken varatun muistin.

## See Also (Katso Myös)
- cJSON GitHub-repo: https://github.com/DaveGamble/cJSON
- Jansson-kirjaston dokumentaatio: http://www.digip.org/jansson/
- JSON-standardin virallinen verkkosivusto: https://www.json.org/json-en.html
