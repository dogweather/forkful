---
title:                "Fish Shell: Tekstitiedoston lukeminen"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Voi tuntua oudolta lukea jopa yksinkertaisesta teksti-tiedostosta. Kuitenkin, jos haluat luoda mukautettuja skriptejä tai tehokkaammin hallita tiedostoja, voi olla hyödyllistä oppia miten voi lukea ja käsitellä tietoa suoraan teksti-tiedostoista.

## Miten

```fish-shell
cat tiedosto.txt
```
Tämä yksinkertainen komento tulostaa tiedoston sisällön terminaaliin. Voit myös käyttää `head` ja `tail` komentoja nähdäksesi vain tiedoston ensimmäisen tai viimeisen osan.

Jos haluat käsitellä tiedoston sisältöä enemmän, voit käyttää komentoja kuten `awk` tai `sed` poimimaan tiettyjä tietoja tai muokkaamaan tekstiä. Esimerkiksi, jos haluat nähdä vain tietyn sarakkeen tiedosto.txt-tiedostosta, voit tehdä sen näin:

```fish-shell
awk '{print $2}' tiedosto.txt
```
Tämä tulostaa toisen sarakkeen jokaiselta riviltä tiedostosta.

## Syvä sukellus

Teksti-tiedoston lukeminen on hyödyllinen taito kun teet skriptejä tai haluat hallita tiedostoja terminaalista. Voit myös käyttää `grep` komentoa hakeaksesi tiettyä tekstiä tiedostosta tai `sed` komentoa muokataksesi tekstiä haluamallasi tavalla.

Voit myös käyttää `pipe` merkkiä (`|`) yhdistämään erilaisia komentoja ja suorittamaan useita toimintoja yhdellä komennolla. Esimerkiksi voit käyttää `cat` komentoa yhdessä `grep` ja `wc` komentojen kanssa laskeaksesi montako kertaa tietty sana esiintyy tiedostossa:

```fish-shell
cat tiedosto.txt | grep "sana" | wc -l
```

Tämä tulostaa kyseisen sanan lukumäärän tiedostossa.

## Katso myös

- [Fish Shell käyttöönotto-opas](https://fishshell.com/docs/current/tutorial.html)
- [Teksti-tiedostot ja komentojen päällekkäisyys](https://forum.ubuntu-fi.org/index.php?topic=47805.0)
- [Awk ja Sed - tekstiä käsittelevät käyttöohjeet](https://developer.ibm.com/articles/l-lp1612/)