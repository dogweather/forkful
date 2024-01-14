---
title:    "Haskell: Merkkijonojen yhdistäminen"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa haluat yhdistää merkkijonoja Haskell-ohjelmoinnissa. Tämä voi olla hyödyllistä esimerkiksi käyttäjän syötteiden tai tietokantatietojen yhdistämiseen ja käsittelyyn.

## Miten

Concatenate-funktio yhdistää kaksi merkkijonoa yhteen. Voit käyttää sitä merkkijonojen tai listojen yhdistämiseen. Katso tästä esimerkki, jossa yhdistämme kaksi merkkijonoa ja tulostamme sen:

```Haskell
concatenate :: String -> String -> String
main = do
  let string1 = "Tervetuloa "
  let string2 = "blogiini!"
  let concatenatedString = concatenate string1 string2
  putStrLn(concatenatedString)
```

Tämä koodi tulostaa:

```
Tervetuloa blogiini!
```

Voit myös käyttää concatenate-funktiota yhdistämään listoja. Katso tästä esimerkki:

```Haskell
concatenateLists :: [String] -> String
main = do
  let words = ["Hei", "sinä", "siellä"]
  let concatenatedWords = concatenateLists words
  putStrLn(concatenatedWords)
```

Tämä koodi tulostaa:

```
Hei sinä siellä
```

## Syvempi sukellus

Concatenate-funktiossa on paljon potentiaalia monimutkaisempiin tehtäviin. Voit esimerkiksi käyttää sitä hienostuneempien merkkijonojen käsittelyyn, kuten kehitsemään kustomoituja toimintoja, jotka yhdistävät merkkijonoja erilaisilla ehdoilla. Voit myös käyttää sitä listojen ja elementtien yhdistämiseen loogisilla ehdoilla. Kokeile rohkeasti ja kehitä omaa tapaasi käyttää concatenate-funktiota.

## Katso myös

- [Haskellin virallinen dokumentaatio yhdistämisfunktioista (englanniksi)](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:++)
- [Tutoriaali merkkijonojen ja listojen käsittelystä Haskellissa (englanniksi)](https://www.tutorialspoint.com/haskell/haskell_lists.htm)
- [Laajempi tutoriaali Haskell-ohjelmoinnista (englanniksi)](https://www.haskell.org/tutorial/)