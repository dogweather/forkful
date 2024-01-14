---
title:                "Haskell: Muunna merkkijono pienaakkosiksi"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Monissa tapauksissa ohjelmointitehtävän yksi osa voi olla muuttaa merkkijono pieniksi kirjaimiksi. Tämä voi olla erityisen hyödyllistä esimerkiksi tietokannassa tai vertailussa. Haskellissa tämän tekeminen voi olla hieman haastavaa, mutta se on ehdottomasti mahdollista ja tämä artikkeli auttaa sinua siinä.

## Miten se tehdään

```Haskell
-- Tuo Data.List moduuli käyttöön
import Data.List

-- Luodaan funktio "lowercaseString", joka ottaa merkkijonon argumenttina ja palauttaa sen pieninä kirjaimina
lowercaseString :: String -> String
lowercaseString x = map toLower x

-- Kutsutaan funktiota ja annetaan sille merkkijono
lowercaseString "TÄMÄ ON MERKKIJONO"  -- palauttaa "tämä on merkkijono"
```

Kuten näet, me käytämme `import`komennon avulla `Data.List` moduuli tässä esimerkissä. Se sisältää `map` funktion, joka on avain tämän tehtävän suorittamiseen. `map` ottaa kaksi argumenttia: ensimmäisenä funktio ja toisena lista. Se soveltaa funktiota jokaiseen alkioon listassa.

Mutta me haluamme tässä tapauksessa käyttää `Data.Char` moduulia, joka sisältää `toLower` funktion, joka muuttaa yhden merkin pieneksi kirjaimeksi. Joten me käytämme `map` funktiota yhdistettynä `toLower` funktioon luodaksemme `lowercaseString` funktion.

## Syväsukellus

Voit myös muuttaa erityisiä kirjaimia pieneksi kirjaimeksi `lowercaseString` funktion sijaan. Esimerkiksi, jos haluat muuttaa ääkköset pieniksi kirjaimiksi, voit käyttää `Data.Set` moduulia ja sen `fromList` ja `toList` funktioita yhdessä `lowercaseString` funktion kanssa.

```Haskell
-- Tuo Data.List ja Data.Set moduuli käyttöön
import Data.List
import Data.Set

-- Luodaan funktio "lowercaseStringWithSpecialChars", joka ottaa merkkijonon argumenttina ja palauttaa sen pieninä kirjaimina
lowercaseStringWithSpecialChars :: String -> String
lowercaseStringWithSpecialChars x = map toLower (Data.Set.toList (Data.Set.fromList x))

-- Kutsutaan funktiota ja annetaan sille merkkijono
lowercaseStringWithSpecialChars "TÄMÄ ON ÄÄKKÖSILLÄ" -- palauttaa "tämä on ääkkösillä"
```

`Data.Set` moduuli mahdollistaa samojen kirjainten poistamisen ja `toList` muuntaa sen takaisin listaksi `lowercaseString` funktion suorittamista varten.

Voit myös ottaa käyttöön muita moduuleja ja käyttää muita toimintoja luodessasi oman `lowercaseString` funktion. Tämä riippuu paljon siitä, mitä haluat saavuttaa ja mitä tarvitset tässä tehtävässä.

## Katso myös

- [Haskellin virallinen dokumentaatio merkkijonojen käsittelystä](https://www.haskell.org/documentation/#strings)
- [Haskellin virallinen dokumentaatio moduuleista](https://www.haskell.org/documentation/#modules)
- [Data.List moduulin dokumentaatio](https://hackage.haskell.org/package/base/docs/Data-List.html)
- [Data.Char moduulin dokumentaatio](https://hackage.haskell.org/package/base/docs/Data-Char.html)
- [Data.Set moduulin dokumentaatio](https://hackage.haskell.org/package/containers/docs/Data-Set.html)