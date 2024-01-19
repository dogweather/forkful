---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hahmojen poistaminen, jotka vastaavat mallia, tarkoittaa tiettyjen merkkijonojen tai merkkien tunnistamista ja niiden hävittämistä. Ohjelmoijat tekevät tämän tiedon suodattamiseksi tai sen jäsentelemiseksi paremmin.

## Kuinka:

Tässä on esimerkkikoodi `tr`-komennon käytöstä (translate) hahmojen poistamisessa:

```Bash
echo "Hello World" | tr -d 'l'
```

Koodin tuloste:

```Bash
Heo Word
```

Tässä komennossa `tr -d 'l'` poistaa kaikki 'l'-kirjaimet syötteenä olevasta merkkijonosta.

## Syvällisempi tarkastelu:

Yksinkertaisimmillaan, komento `tr` on ollut käytössä Unix-järjestelmissä 1970-luvun alusta saakka. Se on yksinkertainen ja tehokas työkalu merkkijonotiedon manipulointiin.

Vaihtoehtoisesti, voit käyttää `sed`-komentoa saman tehtävän suorittamiseen:

```Bash
echo "Hello World" | sed 's/l//g'
```

Tämä `sed`-komento toimii samalla tavalla, poistaen kaikki 'l'-kirjaimet.

On huomattava, että `tr` ja `sed`-komennon käyttötavat eroavat hieman. `tr` käsittelee merkit erikseen, kun taas `sed` voi työskennellä koko merkkijonojen, tai niin sanottujen mallien, kanssa.

## Katso myös:

Seuraavat linkit ovat hyödyllisiä, jos haluat tietää enemmän `tr` tai `sed`-komennoista:

- GNU coreutils `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Sed-käyttäjän oppaan: https://www.gnu.org/software/sed/manual/sed.html
- Regexone, opas säännöllisiin lausekkeisiin: https://regexone.com/references/learn_regex/bash

Muista aina käyttää sopivaa työkalua kuhunkin tehtävään!