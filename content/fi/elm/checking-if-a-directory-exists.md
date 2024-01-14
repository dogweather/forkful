---
title:    "Elm: Tarkistetaan löytyykö kansio"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemiston olemassaolo

On monia syitä, miksi voit tarkistaa, onko hakemisto olemassa. Esimerkiksi, jos haluat tallentaa ja lukea tiedostoja, on tärkeää varmistaa, että haluamasi hakemisto on olemassa ennen kuin yrität tehdä mitään sen sisällä. Tämä auttaa välttämään virheitä ja parantaa ohjelman suorituskykyä.

## Kuinka tehdä se

Tarkistaminen, onko hakemisto olemassa Elm-ohjelmoinnissa, on melko yksinkertaista. Käytämme Elm core -kirjaston `Directory` -moduulia, joka tarjoaa toiminnon `doesDirectoryExist` olemassaolotarkistukseen. Tässä on esimerkki:

```Elm
import Directory exposing (doesDirectoryExist)

checkDirectory : String -> Cmd Msg
checkDirectory dir =
  doesDirectoryExist dir
    |> Task.perform DirectoryResult
```

Ilmaisimme ensin `Directory` -moduulin ja sen jälkeen käytimme toimintoa `doesDirectoryExist`, joka lähettää ulostuloksi `Bool`-arvon osoittaen, onko hakemisto olemassa vai ei. Sitten suoritamme tehtävän `Task.perform` tulosarvolla `DirectoryResult`.

`DirectoryResult` on vain yksinkertainen tyyppien määrittämä merkki, jolla varmistamme, että saamme oikean tyyppisen arvon. Tässä on tulosarvo esimerkkiohjelman ajon jälkeen:

```
{ ok = True }
```

Tai jos hakemistoa ei ole olemassa, tulostus on seuraava:

```
{ ok = False }
```

## Syvempää tietoa

Vaikka `doesDirectoryExist` on huomattavasti helpompi käyttää kuin muut vaihtoehdot, sitä on hyvä tietää myös kaksi muuta toimintoa: `getDirectory` ja `directoryExists`. Nämä antavat lisätietoa hakemiston sijainnista ja sen olemassaolosta. Voit tutustua niiden dokumentaatioon lisää täällä:

- [getDirectory](https://package.elm-lang.org/packages/elm/core/latest/Directory#getDirectory)
- [directoryExists](https://package.elm-lang.org/packages/elm/core/latest/Directory#directoryExists)

## Katso myös

Tässä muutamia hyödyllisiä linkkejä, jotka liittyvät hakemiston tarkistamiseen Elm-ohjelmoinnissa:

- [Elmin standardikirjasto: hakemistot](https://package.elm-lang.org/packages/elm/core/latest/Directory)
- [The Elm Architecture](https://guide.elm-lang.org/architecture/)
- [Elm Guide](https://guide.elm-lang.org/)