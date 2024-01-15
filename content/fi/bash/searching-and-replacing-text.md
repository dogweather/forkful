---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Bash: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko puhdistaa tekstiä tai korvata tiettyjä sanoja tai lausekkeita jokaisesta tiedostosta? Etsiminen ja korvaaminen on kätevä keino tehdä tämä Bash-komentojonon avulla.

## Miten

Etsiminen ja korvaaminen voi olla hyödyllistä esimerkiksi skriptien tekemisessä. Voit käyttää komentoa "sed" etsimään tiettyä sanaa ja korvaamaan sen toisella sanalla. Seuraava esimerkki korvaa sanan "kissa" sanalla "koira" tiedostossa "eläimet.txt":

```Bash
sed -i 's/kissa/koira/g' eläimet.txt
```

Tämä korvaa kaikki esiintymät sanasta "kissa" tiedostossa "eläimet.txt" sanalla "koira".

Voit myös suorittaa etsinnän ja korvaamisen käyttämällä Bashin muuttujia. Seuraavassa esimerkissä korvaamme sanan "koti" muuttujalla "talo" tiedostossa "osoitteet.txt":

```Bash
koti="talo"
sed -i "s/koti/$talo/g" osoitteet.txt
```

Tässä komennossa muuttuja "koti" nimetään ensin arvolle "talo" ja sitten Bash korvaa kaikki esiintymät sanasta "koti" tiedostossa "osoitteet.txt" arvolla "talo".

## Syvempi sukellus

"sed" -komennon lisäksi Bashilla on muita työkaluja etsimiseen ja korvaamiseen, kuten "awk" ja "grep". Nämä komennot tarjoavat lisää vaihtoehtoja ja mukauttamismahdollisuuksia, jotka voivat olla hyödyllisiä joissakin skenaarioissa.

Voit myös yhdistää etsimisen ja korvaamisen useampaan komentoon yhdellä rivi-, kuten tässä esimerkissä:

```Bash
cat tiedosto.txt | sed 's/alas ylös/' | sort | uniq > uusi_tiedosto.txt
```

Tämä komento suorittaa etsimisen ja korvaamisen tiedostossa "tiedosto.txt", sitten järjestää ja poistaa duplikaatit ja tallentaa muutetun tiedoston "uusi_tiedosto.txt".

## Katso myös

- [Bash Basics: Using sed to Find and Replace Text](https://linuxize.com/post/bash-find-and-replace-text-string/)
- [Bash's official documentation for sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Other text manipulation tools in Bash](https://www.fosslinux.com/3164/12-examples-of-special-bash-variables-every-linux-user-should-know.htm#text-manipulation)