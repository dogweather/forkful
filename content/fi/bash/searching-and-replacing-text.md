---
title:    "Bash: Tekstin etsiminen ja korvaaminen"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointityössä joutuu tekemään suuria määriä tekstinhakuja ja -korvauksia tiedostoista tai dokumenteista. Näihin tilanteisiin on olemassa kätevä ratkaisu Bash-skriptien avulla, joka säästää aikaa ja vaivaa manuaaliseen tekstin muokkaamiseen.

## Kuinka

Bash-skriptin avulla voit tehdä tekstinhakuja ja -korvauksia käyttäen ```sed```-komennon ```s```-toimintoa. Tässä esimerkki, jossa etsitään sanaa "moi" ja korvataan se sanalla "hei":

```Bash
sed 's/moi/hei/g' tiedosto.txt
```

Tämä korvaa kaikki esiintymät sanasta "moi" tiedostossa "tiedosto.txt" sanalla "hei" ja tulostaa muokatun version suoraan terminaaliin.

Voit myös korvata tekstiä vain tietyistä kohdista tiedostossa lisäämällä numeron ennen korvauskomennon ```g```-päätettä. Esimerkiksi, jos haluat korvata vain ensimmäisen esiintymän sanasta "moi", voit käyttää seuraavaa koodia:

```Bash
sed 's/moi/hei/1' tiedosto.txt
```

## Syvempi sukellus

Bash-skriptien avulla on mahdollista tehdä monimutkaisiakin hakuja ja korvauksia. Voit esimerkiksi korvata erityyppisiä merkkejä, muuttaa ryhmiä tai käyttää säännöllisiä lausekkeita tekstinhakuihin.

Lisäksi on hyvä huomioida, että ```sed```-komennon käytössä on tärkeää käyttää oikeita erikoismerkkejä, kuten kauttaviivoja (/) tai pistettä (.), jos niitä halutaan korvata.

Lisätietoa Bash-skripteistä ja ```sed```-komennosta löydät esimerkiksi seuraavista linkeistä:

- https://www.gnu.org/software/sed/
- http://tldp.org/LDP/abs/html/textproc.html
- https://www.tutorialspoint.com/sed/

## Katso myös

- https://www.linuxjournal.com/content/global-find-and-replace-using-sed
- https://www.viur.is/blog/bash-scripting-find-and-replace-text-files
- http://www.robelle.com/tips/sed_syntax.html