---
title:    "Elm: Väliaikaisen tiedoston luominen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi: 

Miksi luoda väliaikainen tiedosto? Väliaikaiset tiedostot ovat hyödyllisiä, kun tarvitset tilapäistä tallennustilaa ohjelmassa, esimerkiksi väliaikaisesti tallentaaksesi käyttäjän syöttämiä tietoja.

## Kuinka: 

```Elm
import File
import Random


main =
    File.withTempPath "temp.txt" <|
        \path ->
            Random.generate 5 10 (File.write path)
```

Koodiesimerkissä käytämme `File`-moduulista löytyvää`withTempPath`-funktiota, joka luo väliaikaisen tiedoston annetulla nimellä ja suorittaa sille annetun toiminnon (`Random.generate` ja `File.write`). Lopuksi väliaikainen tiedosto poistetaan automaattisesti.

```
temp.txt
8
10
2
3
9
```

Yllä olevassa esimerkissä `withTempPath` luo tiedoston nimeltä `temp.txt` ja kirjoittaa sinne viisi satunnaista lukua välillä 5-10. Tämän jälkeen tiedosto poistetaan ja konsolille tulostuu tiedostossa olleet luvut.

## Syväreissaus: 

Väliaikaisten tiedostojen luomiseen liittyy useita tekijöitä, jotka voivat vaikuttaa ohjelman ajamiseen ja suorituskykyyn. On tärkeää muistaa poistaa väliaikainen tiedosto ohjelman suorituksen jälkeen, jotta se ei aiheuta ylimääräistä tilaa tai mahdollisia tietoturvariskejä.

## Katso myös: 

- [Elm File -documentation](https://package.elm-lang.org/packages/elm/file/latest/)
- [Random -documentation](https://package.elm-lang.org/packages/elm/random/latest/)
- [Understanding Temporary Files in Programming](https://www.geeksforgeeks.org/understanding-temporary-files-in-programming/)