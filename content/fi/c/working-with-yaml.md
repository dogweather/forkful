---
title:                "Työskentely yaml:in kanssa"
html_title:           "C: Työskentely yaml:in kanssa"
simple_title:         "Työskentely yaml:in kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-yaml.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

YAML on yleisesti käytetty tiedostonmuoto, jota käytetään tietojen tallentamiseen ja jakamiseen ohjelmistojen välillä. Se on suunniteltu ihmisluettavaksi ja helppokäyttöiseksi, joten se on yleinen valinta monilla ohjelmoijilla. YAML-tiedostoja voidaan myös käyttää konfigurointitiedostoina ohjelmistoissa.

# Miten:

Koodiesimerkit ja näytetyt tulosteet voit löytää alla olevista ```C ... ``` koodilohkoista.

### Tiedoston lukeminen:

```C
//esimerkki tiedoston lukemisesta
#include <stdio.h>
#include <stdlib.h>
#include <yaml.h>

int main(int argc, char *argv[]) {
    FILE *fh = fopen("esimerkki.yaml", "rb");

    yaml_parser_t parser;
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, fh);

    /* Tässä voit käsitellä tiedoston sisältöä */
    
    yaml_parser_delete(&parser);
    return 0;
}
```

### Tiedoston kirjoittaminen:

```C
//esimerkki tiedoston kirjoittamisesta
#include <stdio.h>
#include <stdlib.h>
#include <yaml.h>

int main(int argc, char *argv[]) {
    FILE *fh = fopen("uusi.yaml", "wb");

    yaml_emitter_t emitter;
    yaml_emitter_initialize(&emitter);
    yaml_emitter_set_output_file(&emitter, fh);

    /* Tässä voit kirjoittaa tiedostoon haluamasi tiedot */
    
    yaml_emitter_delete(&emitter);
    return 0;
}
```

### Tiedostosta lukeminen ja siihen kirjoittaminen:

```C
//esimerkki tiedostosta lukemisesta ja siihen kirjoittamisesta
#include <stdio.h>
#include <stdlib.h>
#include <yaml.h>

int main(int argc, char *argv[]) {
    FILE *input = fopen("vanha.yaml", "rb");
    FILE *output = fopen("uusi.yaml", "w");

    yaml_parser_t parser;
    yaml_parser_initialize(&parser);
    yaml_parser_set_input_file(&parser, input);

    yaml_emitter_t emitter;
    yaml_emitter_initialize(&emitter);
    yaml_emitter_set_output_file(&emitter, output);

    /* Tässä voit käsitellä tiedoston sisältöä ja kirjoittaa tiedostoon haluamasi tiedot */

    yaml_parser_delete(&parser);
    yaml_emitter_delete(&emitter);

    return 0;
}
```

# Syvemmälle:

YAML (Yet Another Markup Language) kehitettiin alun perin vuonna 2001, ja sen tavoitteena oli korvata XML-tiedostot helpommin luettavalla ja kirjoitettavalla muodolla. Se on saavuttanut suosiota erityisesti konfigurointitiedostojen parissa, ja sitä käytetään laajalti esimerkiksi Dockerissa ja Ansiblessa.

# Katso myös:

- [YAML.org](https://yaml.org/): YAML:n virallinen verkkosivusto
- [Learn X in Y minutes - YAML](https://learnxinyminutes.com/docs/yaml/): Selkeä ja nopea opas YAML:n käyttöön
- [Stack Overflow - YAML](https://stackoverflow.com/questions/tagged/yaml): Kattava kokoelma kysymyksiä ja vastauksia YAML:stä Stack Overflow -sivustolla.