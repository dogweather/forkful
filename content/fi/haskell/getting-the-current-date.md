---
title:    "Haskell: Nykyisen päivämäärän saaminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi perehtyä nykyisen päivämäärän hakemiseen?

Päivämäärän hakeminen on tärkeä askel monessa ohjelmoinnissa, varsinkin silloin kun ohjelmaan halutaan lisätä ajastettuja toimintoja tai päivämäärän käsittelyä. Haskellin tarjoamat työkalut tekevät tästä tehtävästä vaivatonta ja tehokasta. Seuraavassa kerrotaan, miten tämä tehtävä voidaan toteuttaa.

## Miten?

Nykyisen päivämäärän hakeminen Haskellissa onnistuu käyttämällä `getCurrentTime`-funktiota. Tämä funktio palauttaa IO-monadissa arvon `UTCTime`, joka sisältää tiedon vuodesta, kuukaudesta, päivästä, tunnista, minuutista ja sekunnista. Tämän tiedon pohjalta päivämäärää voidaan käsitellä ja muokata tarpeen mukaan.

```Haskell
import Data.Time.Clock

getCurrentDate :: IO UTCTime
getCurrentDate = getCurrentTime

main :: IO ()
main = do
  currentDate <- getCurrentDate
  putStrLn $ "Nykyinen päivämäärä on: " ++ show currentDate
```

Tässä esimerkissä `getCurrentTime`-funktiota käytetään yhdessä `putStrLn`-funktion kanssa tulostamaan nykyinen päivämäärä konsoliin. Huomaa, että `IO UTCTime` täytyy paketoida `show`-funktion avulla saadakseen sen muotoon, joka voidaan tulostaa.

## Syvemmälle aiheeseen

Vaikka `getCurrentTime`-funktion käyttäminen onkin helppoa, voi päivämäärän käsittelyllä olla myös haasteita. Esimerkiksi eri aikavyöhykkeiden huomioiminen tai päivämäärän muotoilu haluttuun formaattiin voivat vaatia lisätyötä. Tässä vaiheessa kannattaa käyttää `Time`-moduulia, joka tarjoaa paljon apufunktioita päivämäärän käsittelyyn.

## Katso myös

- [Haskellin virallisten dokumentaatioiden opas päivämäärän käsittelyyn](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/time-1.9.3/Data-Time.html)
- [Haskell-wikin artikkeli päivämäärän käsittelystä](https://wiki.haskell.org/Date_and_time)