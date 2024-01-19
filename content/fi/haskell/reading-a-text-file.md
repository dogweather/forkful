---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston lukeminen on prosessi, jossa ladataan tiedot tiedostosta ohjelmaan tai muistitilaan. Koodaajat tekevät tämän datan käsittelyä varten.

## Miten Näin?

Haskellin lukutoiminnot ovat yksinkertaisia. Voit lukea tekstitiedoston seuraavasti:

```Haskell
-- Ottaa tiedosto "teksti.txt", lukee sen ja tulostaa sisällön

main = do  
    sisalto <- readFile "texti.txt"  
    putStrLn sisalto
```

Tämän koodin suorittaminen lukee "teksti.txt" -tiedoston ja tulostaa sisällön.

## Syvempi Sukellus

Historiallisessa kontekstissa, Haskellin lukutoimintojen suunnittelu pohjautuu Unixin filosofiaan, että "kaikki on tiedosto". Se tarjoaa yksinkertaisen rajapinnan tiedostojen käsittelyyn.

Vaihtoehtoja tiedostojen lukemiseen Haskellissa on monia. Voidaan käyttää byrokirjastoja kuten `ByteString` tai `Text` tehokkaaseen tiedostonkäsittelyyn, erityisesti suurille tiedostoille.

Haskell lukee tiedoston 'laiskasti'. Tämä tarkoittaa että se ei lataa kaikkea kerralla, mutta paloittelee sen pienempiin osiin. Tämä tekee suurten tiedostojen käsittelyn mahdolliseksi ilman suuren määrän muistin kaeyttöä.

## Katso Myös

Lisätietoja Haskellin tiedostonlukutoiminnoista löydät seuraavista lähteistä:

- [Haskell-tiedostonluku opetusohjelma](https://www.haskell.org/tutorial/io.html)
- [Haskell ByteString-paketti](http://hackage.haskell.org/package/bytestring)
- [Haskell Text-paketti](http://hackage.haskell.org/package/text)

Seuraavaksi suosittelen tutustumaan Haskellin tiedostonkirjoitusfunktioihin, jotka toimivat yhdessä lukutoimintojen kanssa tiedostojen manipuloimiseksi.