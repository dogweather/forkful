---
title:                "Elm: Tarkistetaan, onko kansio olemassa"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointiprojekteissa on tarve tarkistaa, onko hakemistoa olemassa ennen kuin sitä käytetään esimerkiksi tiedostojen tallentamiseen tai lukemiseen. Tämä auttaa välttämään virheitä ja parantaa sovelluksen luotettavuutta.

## Kuinka tehdä

Tässä esimerkissä käytämme Elm-kielen `File` -pakettia, joka tarjoaa erilaisia toimintoja tiedostojen käsittelyyn. Ensimmäiseksi tuomme `File` -paketin käyttöön `import`-ilmaisulla:

```Elm
import File
```

Sitten voimme käyttää `File` -pakettia tarkistaaksemme onko haluttu hakemisto olemassa `File.exists` -funktiolla. Tässä esimerkissä tarkistamme, onko `assets` nimistä hakemistoa olemassa:

```Elm
File.exists "assets" 
```

Jos kyseinen hakemisto on olemassa, tulostamme `True`, muuten `False`:

```Elm
True
```

## Syvien vesien sukeltaminen

Tarkistaaksemme onko hakemisto olemassa, `File.exists` -funktio käyttää `Result` -tyyppiä, joka voi olla joko `Ok` tai `Err` riippuen siitä, onko toiminto onnistunut vai ei. Jos hakemisto on olemassa, `File.exists` palauttaa `Ok True`, muuten `Ok False`.

## Katso myös

- [File-paketin dokumentaatio](https://package.elm-lang.org/packages/elm/file/latest/)
- [Elm-opas (suomeksi)](https://guide.elm-lang.org/)
- [Hakemistojen käsittely Elm-kielen avulla](https://www.freecodecamp.org/news/how-elm-deals-with-directories-b7fbd627bd72/)