---
title:    "Bash: Tarkista, onko hakemistoa olemassa"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi tarkistaa tiedostopolun olemassaolo?

Tiedoston tai kansion olemassaolon tarkistaminen on tärkeä osa Bash-ohjelmointia ja auttaa varmistamaan, että ohjelmasi suorittaa toivotut toiminnot oikeissa poluissa ja tiedostoissa. Tämä auttaa myös estämään virheitä, jotka voivat johtua olemattomista tiedostoista tai kansionimistä.

## Miten tarkistaa kansion olemassaolo Bashissa?

Kansion olemassaolon tarkistamiseksi Bashissa voidaan käyttää `test`-komennolla `-d`-tunnusta, joka tarkistaa, onko annetun polun pääte kansiona vai ei. Voimme myös käyttää `if`-lauseketta ja `&&`-operaattoria tarkistaaksemme olemassaolon ja suorittaa halutut toimenpiteet sen mukaan.

```Bash
if test -d /kansio/olemassa; then
  echo "Kansio on olemassa."
fi
```

Jos kansio ei ole olemassa, mikään ei tulostu.

Voimme myös käyttää `test`-komennon `-e`-tunnusta, joka tarkistaa, onko tiedosto tai kansio olemassa. Tämä on hyödyllistä silloin, kun haluamme tarkistaa useita tiedostoja ja kansioita kerralla.

```Bash
if test -e /kansio/olemassa; then
  echo "Kansio tai tiedosto on olemassa."
fi
```

## Syvempää tietoa tiedostopolun olemassaolon tarkistamisesta

Kansion olemassaolon tarkistamiseen Bashissa on useita eri tapoja. Lisäksi voidaan käyttää myös `[[`-lauseketta, joka on vähemmän herkkä virheille ja tarjoaa enemmän vaihtoehtoja, kuten tyhjän merkkijonon tarkistamisen ja yhtäsuuruuden tarkistamisen.

Tärkeää on myös muistaa, että kansion olemassaolon tarkistaminen ei välttämättä tarkoita, että käyttäjällä olisi oikeudet kansion sisällön tarkastelemiseen tai muokkaamiseen. Tätä varten tulisi käyttää `test`-komennon `-r`-tunnusta, joka tarkistaa, onko käyttäjällä lukuoikeudet valittuun tiedostoon tai kansioon.

## Katso myös

- [Shellscript - Tiedostopolun tarkistaminen](https://www.shellscript.sh/tips/directory_exists/)

- [Tarkista tiedoston olemassaolo Bashissa](https://javarevisited.blogspot.com/2019/03/how-to-check-if-file-or-directory-exists.html)