---
title:                "Haskell: Tarkistaako hakemisto on olemassa"
simple_title:         "Tarkistaako hakemisto on olemassa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemisto olemassa?

Välillä ohjelmoinnissa tarvitsemme tarkistaa, onko tietyssä paikassa olevaa hakemistoa olemassa. Tämä voi johtua esimerkiksi siitä, että haluamme tiedostoon tallentaa jotain, mutta ennen sitä haluamme varmistaa, että hakemisto on olemassa. Tässä artikkelissa käymme läpi, kuinka voimme tarkistaa Haskell-kielellä, onko hakemisto olemassa ja miten voimme käsitellä tilanteita, joissa se ei ole.

## Kuinka tehdä se

Haskell-kielessä on valmiina `doesDirectoryExist` -niminen funktio, joka tarkistaa hakemiston olemassaolon ja palauttaa boolean-arvon. Alla olevassa esimerkissä tarkistetaan, onko hakemisto "testihakemisto" olemassa ja sen jälkeen tulostetaan tulos.

```Haskell
import System.Directory

main = do
    let directory = "testihakemisto"
    exists <- doesDirectoryExist directory
    putStrLn $ "Hakemisto " ++ directory ++ " on olemassa: " ++ show exists
```

Jos hakemisto löytyy, tulostuu `True`, muuten `False`.

## Syvempi sukellus

Tarkoituksessa tarkistaa hakemiston olemassaolo, voimme myös käyttää `doesPathExist` -nimistä funktiota, joka tarkistaa kaikenlaisen polun olemassaolon. Tämä on hyödyllistä silloin, kun haluamme tarkistaa jonkin tietyn tiedoston olemassaolon hakemistossa.

Tässä esimerkissä tarkistamme, onko tiedosto "testitiedosto.txt" olemassa hakemistossa "testihakemisto".

```Haskell
import System.Directory

main = do
    let directory = "testihakemisto"
        file = "testitiedosto.txt"
        path = directory ++ "/" ++ file
    exists <- doesPathExist path
    putStrLn $ "Tiedosto " ++ path ++ " on olemassa: " ++ show exists
```

Kuten edellisessä esimerkissä, tulostus riippuu siitä, löytyykö tiedosto vai ei.

## Katso myös

- [Haskellin System.Directory-dokumentaatio](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Stack Overflow -kuinka tarkistaa, onko hakemisto olemassa Haskelliin](https://stackoverflow.com/questions/646282/how-to-determine-if-a-file-is-a-directory-in-haskell)