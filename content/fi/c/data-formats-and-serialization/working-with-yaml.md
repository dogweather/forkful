---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:45.374978-07:00
description: "Miten: Ty\xF6skentely YAMLin kanssa C-kieless\xE4 vaatii kirjaston,\
  \ sill\xE4 vakio C-kirjasto ei tarjoa suoraa tukea YAMLin j\xE4sent\xE4miselle tai\
  \ serialisoinnille.\u2026"
lastmod: '2024-03-13T22:44:57.062833-06:00'
model: gpt-4-0125-preview
summary: "Ty\xF6skentely YAMLin kanssa C-kieless\xE4 vaatii kirjaston, sill\xE4 vakio\
  \ C-kirjasto ei tarjoa suoraa tukea YAMLin j\xE4sent\xE4miselle tai serialisoinnille."
title: "Ty\xF6skentely YAML:n kanssa"
weight: 41
---

## Miten:
Työskentely YAMLin kanssa C-kielessä vaatii kirjaston, sillä vakio C-kirjasto ei tarjoa suoraa tukea YAMLin jäsentämiselle tai serialisoinnille. Yksi suosituimmista YAML-kirjastoista C:lle on `libyaml`, joka tarjoaa sekä matalan tason että korkean tason rajapintoja YAMLin jäsentämiseen ja tuottamiseen. Alla on esimerkki siitä, kuinka jäsentää yksinkertainen YAML-tiedosto käyttäen `libyaml`-kirjastoa:

**Ensimmäiseksi**, sinun täytyy asentaa `libyaml`-kirjasto. Jos käytät Unix-tyyppistä järjestelmää, voit yleensä asentaa sen paketinhallintasi kautta. Esimerkiksi Ubuntussa:

```bash
sudo apt-get install libyaml-dev
```

**Seuraavaksi**, harkitse yksinkertaista YAML-tiedostoa nimeltä `config.yaml`:

```yaml
nimi: John Doe
ikä: 29
naimisissa: false
```

**Tässä** on perusesimerkki siitä, kuinka jäsentää tämä YAML-tiedosto C:ssä:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("YAML-jäsentimen alustus epäonnistui!\n", stderr);

    if (fh == NULL)
        fputs("Tiedoston avaus epäonnistui!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Arvo: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Tämä yksinkertainen ohjelma avaa YAML-tiedoston, alustaa YAML-jäsentimen ja lukee tiedoston, tulostaen skalaariarvot (tässä esimerkissä meidän yksinkertaisen YAMLin kentät). Huomaa, että virheen tarkistus on minimaalista tässä yksinkertaisessa esimerkissä ja sen pitäisi olla robustimpi tuotantokoodissa.

Ohjelman suorittaminen `config.yaml`-tiedostollamme tuottaa tuloksen:

```plaintext
Arvo: John Doe
Arvo: 29
Arvo: false
```

## Syväsukellus
YAML julkaistiin ensimmäisen kerran vuonna 2001 ja se on suunniteltu olemaan luettavampi ja käyttäjäystävällisempi kuin muut datan serialisointiformaatit, kuten XML tai JSON, lainaten useista kielistä, kuten C, Perl ja Python, sen suunnittelu filosofiaansa. Siitä huolimatta, että YAML:ssä on etuja luettavuudessa ja helpossa ihmisen tekemässä muokkauksessa, YAML voi olla monimutkainen jäsentää ohjelmallisesti sen sisennyksen käytön ja laajan ominaisuuskokonaisuuden vuoksi, mukaan lukien viittaukset ja mukautetut tyypit.

Vaikka `libyaml` tarjoaa vankan, matalan tason pääsyn YAMLin jäsentämiseen ja tuottamiseen C:ssä, se voi olla hankalaa yksinkertaisissa tehtävissä sen verbose API:n vuoksi. Näistä syistä jotkut ohjelmoijat haluavat käyttää korkeamman tason kirjastoja tai jopa muita datan serialisointiformaatteja, kuten JSONia, työskennellessään C:ssä, erityisesti kun suorituskykyinen jäsentäminen minimaalisella koodin kuormituksella on prioriteetti. Kuitenkin YAML pysyy suosittuna valintana konfiguraatiotiedostoille ja tilanteissa, joissa ihmisen luettavuus on ensisijainen. Vaihtoehtoja, kuten TinyYAML tai korkeamman tason tulkkiin upottaminen (esim. Pythonin tai Luan upottaminen), voisi tarjota enemmän mukavuutta tietyissä sovelluksissa, tasapainotellen käytön helppouden ja suorituskyvyn tarpeiden välillä.
