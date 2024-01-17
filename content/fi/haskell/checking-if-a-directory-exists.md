---
title:                "Tarkistetaan tiedostokansion olemassaolo"
html_title:           "Haskell: Tarkistetaan tiedostokansion olemassaolo"
simple_title:         "Tarkistetaan tiedostokansion olemassaolo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Tiedoston on tärkeä osa ohjelmointia varmistaessa, että hakemisto on olemassa. Näin varmistetaan, että tiedostoja voidaan luoda ja käsitellä onnistuneesti. Ohjelmoijat tekevät tämän tarkistaakseen, että oikeat tiedostot ovat käytettävissä ja välttääkseen virheitä tiedostojen käsittelyssä.

# Kuinka tehdä:
```Haskell
import System.Directory

checkDirectory :: FilePath -> IO Bool
checkDirectory path = do
    exists <- doesDirectoryExist path
    return exists

main :: IO ()
main = do
    let directory = "test" -- replace with desired path
    exists <- checkDirectory directory
    if exists
        then putStrLn "Hakemisto on olemassa."
        else putStrLn "Hakemistoa ei ole olemassa."
```

Suoritus:

```
>> Hakemistoa ei ole olemassa.
```
# Syvällinen Syventyminen:
## Historiallinen Konteksti:
Tiedostojen käsittely on ollut merkittävä osa ohjelmointia jo pitkään. Varmistaminen, että hakemisto on olemassa, on välttämätöntä, jotta ohjelmat voivat toimia luotettavasti ja käsitellä tiedostoja oikein.

## Vaihtoehtoja:
On olemassa useita tapoja tarkistaa, onko hakemisto olemassa. Yksi vaihtoehto on käyttää UNIX-järjestelmän komentoa "ls", joka listaa tiedostot hakemistossa. Toinen vaihtoehto on käyttää hakemiston ominaisuuksia, kuten "isDirectory", joka palauttaa totuusarvon, onko kyseessä hakemisto.

## Toteutus:
Tarkistaaksesi, onko hakemisto olemassa, käytetään "doesDirectoryExist" -funktiota, joka on osa "System.Directory" -kirjastoa. Tämä funktio palauttaa "Bool" -arvon, joka ilmaisee, onko hakemisto olemassa vai ei.

# Katso myös:
- [System.Directory - Haskellin virallinen dokumentaatio](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Tiedostojen käsittely - Haskellin virallinen dokumentaatio](https://www.haskell.org/tutorial/io.html#files-and-directories)