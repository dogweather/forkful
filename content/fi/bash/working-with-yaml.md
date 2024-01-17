---
title:                "Työskentely yaml:n kanssa"
html_title:           "Bash: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
YAML on ihmislukuisen tietojen esittämistä varten. Se on myös helposti luettava, mikä tekee siitä suositun monien ohjelmoijien keskuudessa.

## Miten:
Esimerkkejä koodista ja näytetyistä tuloksista ``Bash ...`` koodilohkojen sisällä.

Esimerkki 1: YAML-tiedoston lukeminen ja tietojen esittäminen Bashilla:
``Bash
read -r tiedosto < text.yml
echo $tiedosto
``
Tulostus: 
``Bash
nimi: Jani
ikä: 28
työ: ohjelmoija
``

Esimerkki 2: Tietojen tallentaminen YAML-tiedostoon Bashilla:
``Bash
eisa järjestelmä: Linux
ohjelmisto: Bash
versio: nykyinen
``

Tulostus tiedostoon:
``Bash
järjestelmä: Linux
ohjelmisto: Bash
versio: nykyinen
``

## Syväsukellus
YAML (Yet Another Markup Language) luotiin vuonna 2001 ja se on muodostunut suosituksi tietojen esittämistä varten ohjelmoijien keskuudessa sen helppokäyttöisyyden ja luettavuuden vuoksi. Muita vaihtoehtoja YAML:lle ovat esimerkiksi XML ja JSON, mutta YAML on usein helpompi käyttää.

YAML koostuu avain-arvo pareista, jotka on erotettu kaksoispisteillä ja välilyönneillä. Tiedostot päättyvät .yml-päätteellä. Bashilla, YAML-tiedostoja voi käsittellä helposti ja niitä voi käyttää tietojen tallentamiseen tai lukemiseen monissa ohjelmissa ja skripteissä.

## Katso myös
- [YAML-tutoriali](https://rollout.io/blog/yaml-tutorial-everything-you-need-get-started/)
- [Bash-ohjelmointi](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [YAML-spesifikaatiot](https://yaml.org/spec/)