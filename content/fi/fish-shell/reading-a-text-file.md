---
title:    "Fish Shell: Tiedoston lukeminen"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lukea tekstitiedostoa? Yleisesti tekstitiedostot sisältävät tärkeitä tietoja ja haluat ehkä käyttää näitä tietoja osana skriptiä tai ohjelmaa.

## Kuinka

```Fish Shell``` -ohjelmointikieli on tehokas työkalu tekstitiedostojen lukemiseen. Voit käyttää ```read var``` -komentoa lukeaksesi tekstitiedoston sisällön ja tallentaa sen muuttujaan. Esimerkiksi:

```
set file (cat example.txt)
echo $file
```

Tässä esimerkissä luemme ```example.txt``` -tiedoston sisällön ja tallennamme sen ```file``` -muuttujaan. Sitten tulostetaan muuttujan sisältö ```echo```-komennolla.

## Syväsukellus

Tiedostojen lukeminen on tärkeä osa ohjelmointia, ja tekstitiedostojen käsittely vaatii usein hieman tarkempaa tietoa. Esimerkiksi voit käyttää ```grep```-komennolla tekstiä sisältävän rivin etsimiseen tiedostosta. Voit myös käyttää ```head``` tai ```tail``` -komentoja tulostaaksesi tiedoston ensimmäisen tai viimeisen rivin.

## Katso myös

- [Fish-komentoriviohjeet](https://fishshell.com/docs/current/index.html)
- [Fish Shell -opetusohjelmat](https://fishshell.com/docs/current/tutorial.html)
- [Vinkkejä Fish Shellin käyttöön](https://www.mankier.com/1/fish)