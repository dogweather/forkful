---
title:    "Elm: Ohjelmointiartikkeli: Virheilmoituksien tulostaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Debug-tulostuksen käyttö on tärkeä osa ohjelmoinnin prosessia varmistaessaan, että ohjelma suorittaa halutun toiminnallisuuden oikein. Se voidaan myös käyttää ongelmien löytämiseen ja korjaamiseen ohjelmoinnin puhdistamisen aikana.

## Miten

Debug-tulostuksen käyttäminen Elm-kielessä on helppoa. Voit käyttää funktiota `Debug.log`, joka hyväksyy kaksi argumenttia - merkkijonon ja minkä tahansa datan tyypin. Merkkijonon tulisi sisältää haluttu viesti ja data tulisi olla haluttu arvo, jota haluat tarkastella. Alla on esimerkki, joka tulostaa "Hei maailma" konsoliin:

```Elm
main = 
    Debug.log "Hei" "maailma"
```

Tämän tulisi tuottaa seuraavanlaisen tulosteen:

```
"maailma" : String
```

## Syvempi sukellus

Debug-tulostuksen avulla voit myös tarkastella monimutkaisempia rakenteita, kuten listoja ja tupleja. Voit käyttää myös pattern matchingia saadaksesi tarkempia tulosteita. Alla on esimerkki, joka tulostaa listan sisältöä:

```Elm
main = 
    let
        lista = [1, 2, 3]
    in
        Debug.log "Lista" lista
```

Tämä tuottaa seuraavan tulosteen:

```
Lista : List number
[1, 2, 3]
```

Kuten näet, `Debug.log` -funktiosta on paljon hyötyä koodin tarkastelussa ja löydät siitä varmasti hyötyä ohjelmoinnin aikana.

## Katso myös

- Elm-kielessä on muitakin hyödyllisiä debuggausvälineitä, kuten `Debug.todo`, joka auttaa muistuttamaan puuttuvista osista koodissa. Lue lisää täältä: https://package.elm-lang.org/packages/elm/core/latest/Debug
- Elm-kielessä on myös muita tapoja valvoa ohjelman suoritusta, kuten ajastimien ja lomakkeiden avulla. Lue lisää täältä: https://guide.elm-lang.org/effects/
- Jos haluat syventyä lisää Elm-kieleen yleisesti, suosittelemme tutustumaan tämän oppaan materiaaleihin: https://elmprogramming.com/