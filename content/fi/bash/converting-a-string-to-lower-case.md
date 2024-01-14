---
title:                "Bash: Muuttaminen merkkijonoksi pienaakkosiksi"
simple_title:         "Muuttaminen merkkijonoksi pienaakkosiksi"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi saattaisit haluta muuttaa merkkijonon pieniksi kirjaimiksi. Yksi yleinen syy voi olla vertailun helpottaminen tai tietokannan kanssa työskentely, jossa kirjainten koko on merkityksellistä.

## Kuinka tehdä

Käytännön esimerkkejä merkkijonon muuttamisesta pieniksi kirjaimiksi Bash-skriptillä:

```Bash
str="TÄMÄ ON TESTI"
lowercase=$(tr '[:upper]' '[:lower]' <<< $str)
echo $lowercase

# Tulostaa: tämä on testi
```

```Bash
array=("EKSKLUSIIVINEN" "PÄÄSY" "BLOGIIN")

for i in "${array[@]}"
do
  lowercase=$(tr '[:upper:]' '[:lower:]' <<< $i)
  echo $lowercase
done

# Tulostaa:
# eksklusiivinen
# pääsy
# blogiin
```

## Syvällisempi tarkastelu

Bash-skripti käyttää tr-komentoa (tai translate), joka vaihtaa merkkijonon kaikki isot kirjaimet pieniksi kirjaimiksi. Se tekee tämän käyttämällä kirjainluokkia, jotka määritellään hakasulkeissa. Esimerkiksi [A-Z] tarkoittaa kaikkia isoja kirjaimia ja [a-z] kaikkia pieniä kirjaimia.

## Katso myös

- [Bashin tr-komento](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- [Kuinka käsitellä merkkijonoja Bashilla](https://linuxconfig.org/how-to-handle-string-data-in-bash)