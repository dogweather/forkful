---
title:    "Elm: Satunnaislukujen generointi"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit käyttää satunnaislukujen generoimista ohjelmoinnissa? Satunnaislukujen avulla voit luoda mielenkiintoisia ja arvaamattomia toimintoja tai pelien mekaniikkoja.

## Miten

Käyttääksesi Elm-kielellä satunnaislukujen generaattoria, sinun tulee ensin tuoda käyttöön Random-moduuli koodissa.

```Elm
import Random
```

Sitten voit luoda kaksi funktiota: yhden, joka generoi tietyn välillä olevia kokonaislukuja ja toisen, joka generoi tietyn välillä olevia liukulukuja.

```Elm
-- Generoi kokonaisluku 1-10 välillä
Random.int 1 10

-- Generoi liukuluku 0-1 välillä
Random.float 0 1
```

Voit myös määrittää haluamasi satunnaislukugeneraattorin koon, jolloin saat aina saman sarjan satunnaislukuja.

```Elm
-- Satunnaislukugeneraattori, jossa on 5 lukua, välinä 0-10
Random.list 5 (Random.int 0 10)
```

## Syvempi sukellus

Random-moduulin lisäksi Elm-kielellä on myös tarkkaan määriteltävissä olevia Random.Generator- ja Random.Value -tyyppejä. Random.Generator -tyyppiä voi käyttää luomaan tarkemmin määritettyjä satunnaislukugeneraattoreita, kun taas Random.Value -tyyppiä voi käyttää satunnaislukujen generoimiseen jo olemassa olevien tyyppien, kuten String ja Char, perusteella.

## Katso myös

- [Elm Random-moduuli](https://package.elm-lang.org/packages/elm/random/latest/Random)
- [Elm Random API](https://elm-lang.org/docs/packages/elm/random/latest/Random)