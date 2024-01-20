---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Poistaminen merkkien mukaan on menetelmä, jolla ohjelmointi kielellä poistetaan merkkejä, jotka vastaavat tiettyä mallia. Ohjelmoijat käyttävät sitä tiedostojen ja koodin siistimiseen tai tärkeiden tietojen erottamiseen.

## Miten Näin:

Voit käyttää Fish Shell -koodia merkkien poistamiseen. Katsotaanpa esimerkkiä:

```Fish Shell
set muuttuja "Hei, Fish Shell on hauskaa!"
echo $muuttuja | string replace -r 'Fish Shell' ''
```

Tulostuu:

```Fish Shell
"Hei,  on hauskaa!"
```

Tässä esimerkissä 'Fish Shell' -merkkijono poistettiin muuttujasta.

## Syvällisempi Sukellus:

Fish Shell, syntyi 2005, on yksi nuoremmista Unix-komentotulkeista. Sen ainutlaatuiset ominaisuudet, kuten tehtävien automaattinen täydentäminen ja kattavat toiminnot merkkijonojen käsittelyyn, tekivät siitä suositun.

Vaihtoehtona voit käyttää `sed` tai `awk` Unix-työkaluja, mutta Fish Shell on erittäin mukava työkalu, erityisesti aloittelijoille.

Mallia vastaavien merkkien poistamisen toteutus perustuu säännöllisten lausekkeiden (regex) käsitteeseen. Fish Shell tukee täydellisesti regexiä, mikä tekee koodista tehokkaampaa ja joustavampaa.

## Katso Myös:

1. Fish Shell ohjekirja: https://fishshell.com/docs/current/index.html
2. Unix Sed-työkalun ohjekirja: https://www.gnu.org/software/sed/manual/sed.html
3. Unix Awk-työkalun ohjekirja: https://www.gnu.org/software/gawk/manual/gawk.html
4. Säännöllisten lausekkeiden oppaan: https://www.rexegg.com/regex-quickstart.html