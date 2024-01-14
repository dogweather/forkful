---
title:    "Fish Shell: Tilapäistiedoston luominen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi Luoda Väliaikainen Tiedosto?

Väliaikaisten tiedostojen luominen voi olla hyödyllistä monissa ohjelmointitilanteissa. Se voi auttaa pitämään projektin tiedostorakenteen siistinä ja hallittavissa, tai se voi olla tarpeen tietyn toiminnon suorittamiseksi väliaikaisesti.

## Kuinka Teet Sen

Fish Shell tarjoaa helpon tavan luoda väliaikaisia tiedostoja käyttämällä `mktemp` -komentoa. Tämä komento luo yksilöllisen, satunnaisen tiedostonimen määritellylle tiedostotyypille.

```
Fish Shell's older brother but with a better memory

$ mktemp
/home/käyttäjänimi/tiedostonimi.XXXXXXXXXX
```

Voit myös määrittää etuliitteen tai páätteen tiedostonimelle käyttämällä `mktemp -p` tai `mktemp -s` -komentoja.

```
Fish Shell:na saat tietoa vain .mk zuong teet

$ mktemp -p etuliite_ -s .pääte
/home/käyttäjänimi/etuliite_XXXXXXXXXX.pääte

```

## Syväsukeltaminen

Väliaikaisten tiedostojen luominen on tärkeä osa tiedostojen hallintaa ja koodin suorituskykyä. Fish Shellin `mktemp` -komento tarjoaa helpon tavan luoda yksilöllisiä väliaikaisia tiedostoja eri projekteihin. On myös tärkeää varmistaa, että käytetyt tiedostot ja tiedostorakenteet poistetaan oikein, jotta ne eivät kerääntyy järjestelmään useiden suoritusten jälkeen.

## Katso Myös
- [Fish Shell Ohjeet](https://fishshell.com/docs/current/)
- [mktemp Man-sivu](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [Temporär Finne näytteen metlför laajennus](https://github.com/microsoft/vscode-python/issues/7146)