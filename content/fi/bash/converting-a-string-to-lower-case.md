---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Bash: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Muuttaminen merkkijonon pienaakkosiksi tarkoittaa sen muuttamista merkeistä, joista osa on isoja kirjaimia ja osa on pieniä, pelkiksi pieniksi kirjaimiksi. Tätä tehdään yleensä siksi, että koodin vertailu ja käsittely olisi helpompaa, ja se auttaa myös välttämään mahdollisia virheitä johtuen kirjainkoon eroista.

## Miten:

```Bash
# Aloitetaan merkkijonolla isoilla kirjaimilla
STRING="TEKSTI ON ISOILLA KIRJAIMILLA"
# Käytetään 'tr' komentoa muuntamaan merkkijono pienaakkosiksi
echo $STRING | tr '[:upper:]' '[:lower:]'
```

Tulostus:
teksti on isoilla kirjaimilla

Tai voit tallentaa muunnetun merkkijonon uuteen muuttujaan:

```Bash
LOWER_STRING=$(echo $STRING | tr '[:upper:]' '[:lower:]')
# Tulostetaan uusi muuttuja
echo $LOWER_STRING
```

Tulostus:
teksti on isoilla kirjaimilla

## Syvällinen sukellus:
Koodin vertailu ja käsittely helpottuu, kun kaikki merkkijonot ovat samassa muodossa. Käyttämällä 'tr' komentoa syntyy uusi merkkijono, jossa kaikki kirjaimet ovat pieniä. On myös mahdollista käyttää muita komentoja muuntamiseen, esimerkiksi 'sed' tai 'awk', mutta 'tr' on yksinkertaisin ja tehokkain tapa tehdä tämä Bashissa.

## Katso myös:
https://www.shellscript.sh/case.html - Opas Bashin erilaisiin muodonmuutoksiin
https://www.tutorialspoint.com/unix_commands/tr.htm - Lisää tietoa 'tr' komennosta ja sen eri käyttötavoista