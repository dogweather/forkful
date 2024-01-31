---
title:                "Työskentely TOML:n kanssa"
date:                  2024-01-26T04:19:38.936594-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely TOML:n kanssa"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML on dataa serialisoiva kieli, joka on suunniteltu olemaan helppolukuinen ja -kirjoitettava. Ohjelmoijat käyttävät sitä konfiguraatiotiedostoihin, yksinkertaiseen datan tallennukseen ja kielienväliseen datanvaihtoon sen selkeyden ja ihmiskeskeisyyden vuoksi.

## Miten:
Käsitellään TOML-konfiguraatiotiedosto C-kielessä käyttäen "tomlc99" kirjastoa. Asenna ensin kirjasto. Luo sitten `config.toml`:

```toml
title = "TOML Esimerkki"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Nyt, jäsennä se C:ssä:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Virhe: konfiguraatiotiedostoa ei voi avata\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Virhe: %s\n", errbuf);
        return 1;
    }

    printf("Otsikko: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Omistajan nimi: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Esimerkkituloste:
```
Otsikko: "TOML Esimerkki"
Omistajan nimi: "Tom Preston-Werner"
```

## Syväsukellus
TOML, joka tarkoittaa Tom's Obvious, Minimal Language (Tomin ilmeinen, minimaalinen kieli), luotiin Tom Preston-Wernerin toimesta vuonna 2013. Se toimii yksinkertaisempana vaihtoehtona formaateille kuten XML ja YAML, keskittyen olemaan enemmän ihmislukuisa ja -kirjoitettava. Vaikka JSON on toinen vaihtoehto, TOML säilyttää rakenteen, joka on ihmisille visuaalisesti helpompi jäsentää, mikä on yksi pääsyistä sen käyttöönottoon konfiguraatiotiedostoissa.

C-kielessä työskentely TOML:n kanssa tarkoittaa jäsenninkirjaston valintaa, koska kieli ei tue sitä natiivisti. Kirjastot, kuten "tomlc99", ovat C99-standardin mukaisia ja tarjoavat API:n TOML-tekstin purkamiseen. Suorituskykyä harkittaessa asianmukainen virheenkäsittely ja muistinhallinta ovat olennaisia, sillä C:ssä ei ole sisäänrakennettua roskienkeräystä.

## Katso myös:
1. TOML-spesifikaatio: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub-repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Dataa serialisoivien formaattien vertailu: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
