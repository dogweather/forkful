---
date: 2024-01-26 01:16:48.865245-07:00
description: "Miten: Katsotaanpa yksinkertaista Bash-skripti\xE4, joka kaipaa uudelleenj\xE4\
  rjestely\xE4. Se on k\xF6mpel\xF6, toistaa koodia ja sit\xE4 on vaikea seurata."
lastmod: '2024-03-13T22:44:56.747402-06:00'
model: gpt-4-0125-preview
summary: "Katsotaanpa yksinkertaista Bash-skripti\xE4, joka kaipaa uudelleenj\xE4\
  rjestely\xE4."
title: Koodin refaktorointi
weight: 19
---

## Miten:
Katsotaanpa yksinkertaista Bash-skriptiä, joka kaipaa uudelleenjärjestelyä. Se on kömpelö, toistaa koodia ja sitä on vaikea seurata:

```Bash
#!/bin/bash
echo "Anna tiedostonimi:"
read filename
if [ -f "$filename" ]; then
    echo "Tiedosto on olemassa."
    count=$(grep -c "foo" "$filename")
    echo "Sana foo esiintyy $count kertaa."
else
    echo "Tiedostoa ei ole olemassa."
fi
```

Selkeyden ja uudelleenkäytettävyyden vuoksi uudelleenjärjestely voisi sisältää funktioiden käyttöönottoa ja virheiden hallinnan parantamista:

```Bash
#!/bin/bash

function tiedosto_on_olemassa() {
    [[ -f "$1" ]]
}

function esiintymien_maara() {
    grep -c "$1" "$2"
}

function main() {
    local filename word count
    echo "Anna tiedostonimi:"
    read -r filename
    echo "Anna etsittävä sana:"
    read -r word

    if tiedosto_on_olemassa "$filename"; then
        count=$(esiintymien_maara "$word" "$filename")
        echo "Sana $word esiintyy $count kertaa."
    else
        echo "Tiedostoa ei ole olemassa." >&2
        exit 1
    fi
}

main "$@"
```

Uudelleenjärjestelty versio käyttää funktioita luettavuuden parantamiseksi ja mahdollistaa potentiaalisen uudelleenkäytön.

## Syväsukellus:
Uudelleenjärjestely ei ole konsepti, joka syntyi Bashin tai edes korkean tason ohjelmointikielten kanssa; se on yhtä vanha kuin ohjelmointi itse. Termini virallistettiin kirjassa "Refactoring: Improving the Design of Existing Code" ("Uudelleenjärjestely: Olemassa olevan koodin suunnittelun parantaminen") Martin Fowlerin toimesta vuonna 1999, keskittyen pääasiassa oliopohjaisiin kieliin.

Bash-skriptauksen kontekstissa uudelleenjärjestely tarkoittaa usein pitkien skriptien jakamista funktioihin, toiston vähentämistä silmukoiden tai ehtolauseiden avulla, ja yleisten sudenkuoppien, kuten välilyöntien käsittelyn unohdus tiedostonimissä, välttämistä. Parempia vaihtoehtoja Bashille liian monimutkaisille skripteille ovat Python tai Perl, jotka tarjoavat parempia tietorakenteita ja virheenkäsittelyä monimutkaisiin tehtäviin.

Bash-spesifinen uudelleenjärjestely on enemmän parhaiden käytäntöjen noudattamista, kuten muuttujien lainausmerkeissä pitäminen, `[[ ]]`:n käyttö testauksessa `[ ]`:n sijaan ja `printf`:n suosiminen `echo`:n sijaan luotettavan tulosteen vuoksi. Toteutuksen yksityiskohdat keskittyvät usein tyylioppaiden noudattamiseen ja työkalujen, kuten `shellcheck`:in käyttöön staattiseen analyysiin yleisten virheiden löytämiseksi.

## Katso myös:
- [Googlen Shell-tyyliopas](https://google.github.io/styleguide/shellguide.html)
- [ShellCheck, staattinen analyysityökalu shell-skripteille](https://www.shellcheck.net/)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line)
