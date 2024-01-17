---
title:                "Tarkista, onko hakemistoa olemassa"
html_title:           "Bash: Tarkista, onko hakemistoa olemassa"
simple_title:         "Tarkista, onko hakemistoa olemassa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Tiedoston olemassaolon tarkistaminen on yksi tapa, jolla Bash-ohjelmoijat voivat varmistaa, että heidän ohjelmansa suorittuvat oikein. Tarkistamalla, onko hakemistoa olemassa ennen sen luomista tai käyttämistä, voidaan välttää virheitä ja mahdollisesti myös tietoturvariskejä.

## Kuinka:

```Bash
if [ -d "/polku/hakemistoon" ]; then
  echo "Hakemisto on jo olemassa."
else
  echo "Hakemistoa ei ole vielä luotu."
fi
```

Esimerkissä käytetään `-d` vaihtoehtoa, joka tarkistaa, onko annettu polku olemassa ja onko se hakemisto. Tämän jälkeen tulostetaan vastaavasti joko "Hakemisto on jo olemassa." tai "Hakemistoa ei ole vielä luotu."

## Syvempi sukellus:

Tiedostojen olemassaolon tarkistaminen on ollut tärkeä osa Bashia jo sen ensimmäisestä julkaisusta lähtien. Nykyään on myös olemassa muita vaihtoehtoja, kuten `test`-komennon käyttäminen tai `[[ ... ]]`-lauseen käyttö. Virhesanomat voidaan myös ohjata `/dev/null`-tiedostoon, jolloin ne eivät näy käyttäjälle.

## Katso myös:

https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_02.html
https://linuxhint.com/test-bash-command/
https://www.cyberciti.biz/faq/bash-test-if-file-is-a-directory/