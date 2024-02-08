---
title:                "TOML:n kanssa työskentely"
date:                  2024-02-03T18:12:49.739765-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML:n kanssa työskentely"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-toml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

TOML (Tom's Obvious, Minimal Language) on konfiguraatiotiedostomuoto, joka on helppolukuinen selkeiden semantiikkojensa ansiosta. Ohjelmoijat käyttävät sitä sovellusten konfiguraatiotiedostoissa, koska sen yksinkertaisuus ja ihmislukuisuus tekevät siitä erinomaisen valinnan muotoihin kuten XML tai JSON verrattuna tietyissä yhteyksissä.

## Miten:

TOML-tiedostojen käsittely C-kielessä vaatii kirjaston, joka pystyy jäsentämään TOML-tiedostoja, koska C:n standardikirjasto ei sisällä tätä toiminnallisuutta. Suosittu valinta on `tomlc99`, kevyt TOML-jäsentäjä C99:lle. Tässä on nopea opas yksinkertaisen TOML-konfiguraatiotiedoston lukemiseen:

Ensiksi, varmista, että sinulla on `tomlc99` asennettuna ja oikein linkitettynä projektissasi.

**Esimerkki TOML-tiedostosta (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

**C-koodi tämän tiedoston jäsentämiseen:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *configFile;
    configFile = fopen("config.toml", "r");
    if (!configFile) {
        perror("Cannot open file");
        return EXIT_FAILURE;
    }

    toml_table_t *config = toml_parse_file(configFile, NULL, 0);
    if (!config) {
        fprintf(stderr, "Error parsing file\n");
        fclose(configFile);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(config, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Tietokantapalvelin: %s\n", server);

        toml_array_t *ports = toml_array_in(database, "ports");
        for (int i = 0; i < toml_array_nelem(ports); i++) {
            int64_t port;
            toml_int_at(ports, i, &port);
            printf("Portti %d: %ld\n", i, port);
        }
    }

    toml_free(config);
    fclose(configFile);
    return EXIT_SUCCESS;
}
```

**Tuloste:**
```
Tietokantapalvelin: "192.168.1.1"
Portti 0: 8001
Portti 1: 8001
Portti 2: 8002
```

## Syväsukellus

TOML:n loi Tom Preston-Werner, GitHubin toinen perustaja, vastauksena hänen havaitsemiinsa rajoituksiin muissa konfiguraatiotiedostomuodoissa. Sen tavoite on olla suoraviivainen ja yksiselitteinen, sekä ihmisten että tietokoneiden luettavissa ja kirjoitettavissa ilman monimutkaisia jäsentämissääntöjä. C-ekosysteemissä TOML ei ole ensisijainen kansalainen kuten se saattaisi olla korkeamman tason kielissä, kuten Rust `serde_toml` tai Python `toml` kanssa, joilla on kirjastoja natiivituen kanssa. Sen sijaan, C-kehittäjien täytyy turvautua ulkoisiin kirjastoihin kuten `tomlc99`, mutta tämä on tyypillistä ottaen huomioon C:n painotuksen minimalismiin ja suorituskykyyn.

Vaikka TOML:ia ylistetään sen selkeydestä, konfiguraatiotiedostomuodon valitsemisessa on tärkeää harkita projektin tarpeita. Skenaarioissa, jotka vaativat monimutkaisempia rakenteita tai vuorovaikutusta web-API:en kanssa, JSON tai jopa YAML saattavat tarjota paremman sovituksen huolimatta niiden lisääntyvästä monimutkaisuudesta. TOML loistaa konfiguraatioissa, joissa luettavuus ja yksinkertaisuus ovat ensisijaisia, ei välttämättä siellä, missä tarvitaan kehittyneimpiä tietorakenteita.
