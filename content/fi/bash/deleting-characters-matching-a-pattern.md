---
title:                "Mallia vastaavien merkkien poistaminen"
html_title:           "Bash: Mallia vastaavien merkkien poistaminen"
simple_title:         "Mallia vastaavien merkkien poistaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Poistaminen merkkejä vastaavaa mallia on prosessi, jossa poistetaan kaikki tekstissä esiintyvät merkit, jotka vastaavat haluttua kaavaa. Tämä on tärkeää ohjelmoijille, jotka haluavat nopeasti ja tehokkaasti muokata tekstipohjaisia tiedostoja.

## Kuinka tehdä se?

Esimerkiksi, jos haluat poistaa kaikki välilyönnit tiedostosta, voit käyttää seuraavaa komentoa:

```Bash
sed 's/ //g' tiedosto.txt
```

Tämä komento käyttää "sed" -työkalua poistamaan kaikki välilyönnit tiedostosta ja tulostaa uuden version alkuperäisestä tiedostosta.

## Syvällinen sukellus

Poistamisen merkkejä vastaava malli on alunperin kehitetty 1970-luvulla Unix-käyttöjärjestelmälle. Sittemmin siitä on tullut tärkeä työkalu ohjelmoijille ja tekstiä käsitteleville ammattilaisille.

On myös muita tapoja poistaa merkkejä vastaava malli, kuten "tr" -työkalu, joka on erikoistunut yksittäisten merkkien poistamiseen ja korvaamiseen. Myös "awk" -työkalulla voi suorittaa monimutkaisempia merkkien poistamisen operaatioita.

Poistamisen merkkejä vastaava malli käyttää säännöllisiä lausekkeita löytääkseen ja poistaakseen merkit halutun kaavan perusteella. Tämä tarkoittaa, että se voi olla hyödyllinen myös monimutkaisemmissa tekstipohjaisissa tiedostoissa, kuten lokeissa ja konfiguraatiotiedostoissa.

## Katso myös

- [sed-komento-opas](https://www.gnu.org/software/sed/manual/sed.html) - virallinen sed-komento-opas GNU-sivustolla
- [tr -man sivu](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html) - man-sivu tr-työkalulle
- [awk-komento-opas](https://www.gnu.org/software/gawk/manual/gawk.html) - virallinen awk-komento-opas GNU-sivustolla