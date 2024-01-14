---
title:                "Bash: Väliaikaisen tiedoston luominen"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi sement tilapäistiedoston luominen kannattaa?

Sement tilapäistiedostojen luominen on tärkeä osa Bash-ohjelmointia monista syistä. Yksi syy voi olla, että tiedoston luominen on välttämätöntä ennen kuin siihen voidaan tallentaa tietoa, kuten tekstitiedosto tai loki-tiedosto.

## Miten sement tilapäistiedosto luodaan?

Seuraavassa esimerkissä luodaan sement tilapäistiedosto ```temp.txt``` ja lisätään siihen tiedot "Tämä on tilapäistiedosto" käyttämällä komentoa ```echo```:

```Bash
touch temp.txt
echo "Tämä on tilapäistiedosto" > temp.txt
cat temp.txt
```

Tämän tulisi antaa seuraava tulos:

```
Tämä on tilapäistiedosto
```

Nyt voit käyttää tätä tiedostoa esimerkiksi tallentamaan väliaikaisia ​​tietoja ohjelmassasi.

## Syvemmälle sement tilapäistiedostojen maailmaan

Sement tilapäistiedoston luominen ei aina ole yksinkertaista, sillä tällaiset tiedostot ovat vain väliaikaisia. Tärkein asia, jonka sinun on pidettävä mielessä, on varmistaa, että sement tilapäistiedosto poistetaan ohjelman suorituksen jälkeen. Tämä voidaan tehdä käyttämällä ```trap``` komentoa, joka varmistaa, että tiedosto poistetaan suorituksen lopussa.

Toinen tärkeä kohta on luoda sement tilapäistiedosto turvallisessa sijainnissa. Bash tarjoaa oletusarvoisen kansiot tai tiedostot, kuten ```/tmp```, joka on tarkoitettu sement tilapäistiedostoille. Tämä on turvallisempi vaihtoehto kuin luoda sement tilapäistiedosto suoraan nykyisen hakemiston sisälle.

## Katso myös

- [GNU Bash manuaali](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash Guide for Beginners (Englanniksi)](https://guide.bash.academy/)