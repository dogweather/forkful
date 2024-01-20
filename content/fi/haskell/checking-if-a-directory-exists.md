---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Haskell: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Tarkistetaanko hakemisto olemassa?

### Mikä & Miksi?

Hakemiston olemassaolon tarkistaminen tarkoittaa, toteamme onko tietty hakemisto olemassa tiedostojärjestelmässämme. Tätä käytetään välttääksemme virheitä koodissa, kun yritämme käsitellä hakemistoa joka ei ehkä ole olemassa.

### Miten:

Haskellissa voit tarkistaa, onko hakemisto olemassa, käyttämällä `System.Directory` -moduulin `doesDirectoryExist`-funktiota. tässä on esimerkki:

```Haskell
import System.Directory

main = do
    let dirPath = "/polku/hakemistoon"
    exists <- doesDirectoryExist dirPath
    if exists
        then print $ dirPath ++ " on olemassa."
        else print $ dirPath ++ " ei ole olemassa."
```
Kun ajat tämän koodin, jos hakemisto on olemassa, ohjelma tulostaa "/polku/hakemistoon on olemassa.". Muutoin ohjelma tulostaa "/polku/hakemistoon ei ole olemassa.".

### Syvempi sukellus:

1. Historiallinen konteksti: Hakemiston olemassaolon tarkistaminen on ikivanha käsitteen kaikissa ohjelmointikielissä. Se on perustarve kun käsittellään tiedostojärjestelmiä.

2. Vaihtoehdot: Voit myös tarkistaa, onko tiedosto olemassa käyttämällä `doesFileExist`-funktiota. Tämä on hyödyllistä, kun työskentelet tiedostojen, eikä hakemistojen, kanssa.

3. Toteutuksen yksityiskohdat: `doesDirectoryExist` ja `doesFileExist` -funktiot toimivat tehokkaasti, sillä ne eivät lue koko hakemiston sisältöä tai tiedostoa. Ne vain tarkistavat, onko kohde olemassa tiedostojärjestelmässä.

### Katso myös:

1. [Haskellin 'System.Directory'-moduulin dokumentaatio](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html): Täältä löydät lisätietoa kaikista moduulin tarjoamista funktioista.

2. [Haskellin opas IO-toiminnoista](https://www.haskell.org/tutorial/io.html): Tässä on toinen ohje, joka keskittyy IO-toiminnallisuuteen Haskellissa, mukaan lukien tiedostojärjestelmätoiminnot.

Toivottavasti tästä artikkelista oli sinulle apua. Hyvää koodausta, Haskell-ystäväni!