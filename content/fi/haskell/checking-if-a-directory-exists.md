---
title:                "Tarkistetaan onko hakemisto olemassa."
html_title:           "Haskell: Tarkistetaan onko hakemisto olemassa."
simple_title:         "Tarkistetaan onko hakemisto olemassa."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Joskus ohjelman täytyy tarkistaa, onko tietty kansio olemassa ennen kuin se voi suorittaa halutun toiminnon, kuten tallentaa tiedostoja. Tämä artikkeli näyttää, miten voit tarkistaa kansion olemassaolon Haskell-ohjelmoinnissa.

## Miten
Ensimmäiseksi, tuodaan `System.Directory` -kirjasto, joka tarjoaa toimintoja kansioihin liittyville operaatioille. 
```Haskell
import System.Directory
```
Nyt voimme käyttää `doesDirectoryExist` -funktiota, joka palauttaa `True`, jos annettu polku johtaa olemassa olevaan kansioon ja muulloin `False`.
```Haskell
doesDirectoryExist :: FilePath -> IO Bool
```
Tässä esimerkissä tarkistetaan, onko "Documents" -kansio olemassa ja tulostetaan tulos:
```Haskell
doesDirectoryExist "Documents"
```
```
True
```
Voit myös tarkistaa alikansion olemassaolon antamalla polun, joka johtaa alikansioon:
```Haskell
doesDirectoryExist "Documents/Pictures"
```
```
True
```
Jos annettu kansio ei ole olemassa, tulostetaan `False`:
```Haskell
doesDirectoryExist "Music"
```
```
False
```

## Syvä Sukellus
`doesDirectoryExist` on osa `System.Directory` -kirjaston `Permissions` -moduulia, joka tarjoaa myös muita toimintoja kansioiden oikeuksien ja ominaisuuksien tarkistamiseen. Tässä muutama esimerkki:
- `getPermissions` -palauttaa `Permissions` -rakenteen, joka sisältää tietoa kansion oikeuksista, kuten luku- tai kirjoitusoikeuksista.
- `executable` -funktio palauttaa `True`, jos annettu polku viittaa suoritettavaan tiedostoon.
- `symbolicLink` -funktio tarkistaa, onko annettu polku symbolinen linkki.

Nämä toiminnot ovat hyödyllisiä monimutkaisemmissa ohjelmissa, joissa täytyy tarkistaa myös kansion ominaisuuksia.

## Katso myös
- [System.Directory dokumentaatio](http://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Permissions moduulin dokumentaatio](http://hackage.haskell.org/package/directory/docs/System-Directory-Permissions.html)
- [Haskellin virallinen dokumentaatio](https://www.haskell.org/documentation/)