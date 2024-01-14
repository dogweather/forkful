---
title:    "Haskell: Muuttaminen merkkijonoksi pieniksi kirjaimiksi"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme muuttaa merkkijonon pieniksi kirjaimiksi? Tämä on hyödyllinen ohjelmointitekniikka, jota voidaan käyttää esimerkiksi alkuperäisten merkkijonojen vertailussa. Lisäksi, pieniksi muutetut merkkijonot ovat usein helpompia käsitellä ja yhdentää yhtenäisesti koodissa.

## Miten tehdä

Käytämme Haskellin `map` -funktiota, joka ottaa parametrinaan muunnosfunktion ja listan. Tässä tapauksessa haluamme muuttaa merkkijonon jokaisen merkin pieneksi, joten käytämme `toLower` -funktiota, joka löytyy Data.Char -moduulista. Alla on esimerkki koodista ja sen tulosteesta.

```Haskell
import Data.Char

toLowerString :: String -> String
toLowerString str = map toLower str

main = do 
  let str = "Tämä on MERKKIJONO."
  print $ "Alkuperäinen merkkijono: " ++ str
  print $ "Muutettu pieniksi kirjaimiksi: " ++ toLowerString str
 
```

Tuloste:

```bash
Alkuperäinen merkkijono: Tämä on MERKKIJONO.
Muutettu pieniksi kirjaimiksi: tämä on merkkijono.
```

## Syvempi sukellus

Haskellissa on useita tapoja muuttaa merkkijono pieneksi kirjainsarjaksi, kuten `map` -funktion lisäksi myös `foldr` -funktio. Tämä antaa enemmän joustavuutta muunnoksen tekemiseen erilaisten muunnosfunktioiden avulla.

Erityisesti on hyvä huomata, että `map` -funktio muuttaa merkkijonon jokaisen merkin erikseen ja palauttaa listan, kun taas `foldr` -funktio yhdistää arvot yhdeksi lopulliseksi arvoksi. Tämä voi vaikuttaa suorituskykyyn ja muistinkäyttöön suurten merkkijonojen kanssa.

## Katso myös

- [Haskellin `map` ja `foldr` funktiot](https://www.tutorialspoint.com/haskell/haskell_list_comprehension.htm)
- [Haskellin Data.Char -moduuli](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)