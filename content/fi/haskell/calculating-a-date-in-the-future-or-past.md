---
title:    "Haskell: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Miksi ihminen haluaisi laskea päivämäärää tulevaisuudessa tai menneisyydessä? Tähän on useita syitä, kuten esimerkiksi suunnitelmien tekeminen, ajanhallinta tai historiallisten tapahtumien tutkiminen.

## Miten tehdä

```Haskell
päivämääräTulevaisuudessa :: Integer -> Integer -> Integer -> [Char]
päivämääräTulevaisuudessa vuosi kuukausi päivä
    | kuukausi == 12 && päivä > 31 = tulevaisuudenPäivämäärä 1 1 (vuosi + 1)
    | kuukausi == 12 && päivä == 31 = show päivä ++ "." ++ show kuukausi ++ "." ++ show vuosi
    | päivä > kuukaudenPituus kuukausi = tulevaisuudenPäivämäärä vuosi (kuukausi + 1) 1
    | otherwise = show päivä ++ "." ++ show kuukausi ++ "." ++ show vuosi

kuukaudenPituus :: Integer -> Integer
kuukaudenPituus kuukausi
    | kuukausi == 2 = 28
    | kuukausi == 4 || kuukausi == 6 || kuukausi == 9 || kuukausi == 11 = 30
    | otherwise = 31
```

Esimerkki käytöstä:

```Haskell
> päivämääräTulevaisuudessa 2021 3 26
"26.3.2021"
```

## Syvempää pohdintaa

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä vaatii tarkkaa osaamista Haskell-ohjelmoinnista. Täytyy ottaa huomioon muun muassa vuoden vaihtuminen ja kuukauden eri pituudet. Hyödyllisiä taitoja tämän tehtävän ratkaisemiseksi ovat ehdollisen lauseen käyttö, rekursion ymmärtäminen ja funktioiden rakentaminen.

## Katso myös

- [Haskell-ohjelmoinnin aloittaminen](https://fi.wikipedia.org/wiki/Haskell)
- [Haskellin opiskelumateriaalit](https://www.haskell.org/documentation/)