---
title:    "Haskell: Tarkistaan löytyykö hakemistoa"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi
Tervetuloa lukemaan uutta blogikirjoitusta Haskell-ohjelmointikielen käytöstä! Tänään keskitymme katsomaan, kuinka voimme tarkistaa, onko kansio olemassa Haskellissa.

Yleensä, kun kirjoitamme ohjelmaa, meidän on varmistettava, että se toimii oikein. Joskus tällainen tarkistus sisältää tietojen lukemisen ja kirjoittamisen tietokantoihin tai muihin tiedostoihin. Mutta se voi myös sisältää tarkistamisen, onko haluttu kansio olemassa. Tässä tapauksessa meidän on varmistettava, että ohjelmamme ei yritä käsitellä jotain, joka ei ole olemassa, ja että se toimii oikein, jos kansio löytyy.

## Kuinka 
Haskellissa on helppo tarkistaa, onko kansio olemassa. Voimme käyttää `doesDirectoryExist`-funktiota, joka on määritelty `System.Directory`-moduulissa. Katsotaanpa yksinkertaista esimerkkiä, jossa haluamme tarkistaa, onko `demo`-niminen kansio olemassa:

```Haskell
import System.Directory

main = do
  exists <- doesDirectoryExist "demo"
  if exists
    then putStrLn "Demo-kansio löytyi!"
    else putStrLn "Demo-kansiota ei löytynyt."
```

Jos ajamme tätä koodia kansiossa, jossa on `demo`-kansio, tulostus näyttää seuraavalta:
```
Demo-kansio löytyi!
```

Voimme myös suorittaa saman tarkistuksen yksinkertaisesti kirjoittamalla `doesDirectoryExist "demo"` GHCi-tulkkiin, jolloin se palauttaa True tai False sen mukaan, löytyykö kansio vai ei.

On hyvä huomata, että tämä funktio tarkistaa vain kansiot, ei tiedostoja. Tiedoston olemassaolon tarkistamiseen käytetään `doesFileExist`-funktiota.

## Syvällistä tietoa
Jos haluat tutustua tarkemmin `doesDirectoryExist`-funktion toimintaan, voit tutustua Haddock-dokumentaatioon sen [virallisella verkkosivustolla](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:doesDirectoryExist). Sieltä löydät esimerkiksi, että funktio käyttää alustariippumatonta kutsua `stat`-järjestelmäkutsua ja että se palauttaa tuloksen IO-operaationa boolean-arvona.

## Nähdään myös
- [System.Directory -dokumentaatio](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Tietoja Haskelista](https://fi.wikipedia.org/wiki/Haskell)
- [Haskelin virallinen verkkosivusto](https://www.haskell.org/)