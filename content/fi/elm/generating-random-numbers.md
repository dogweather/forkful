---
title:    "Elm: Satunnaislukujen luominen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Miksi käyttää satunnaisia lukuja koodattaessa?

Satunnaiset luvut ovat tärkeitä monissa ohjelmoinnin sovelluksissa, kuten pelien tekemisessä tai satunnaisten tietojoukkojen generoimisessa. Elm-kielellä satunnaisia lukuja voidaan generoida helposti käyttämällä Random-kirjastoa.

## Kuinka generoida satunnaisia lukuja Elm-kielellä?

Käytä `Random`-moduulia `generateInt`-funktion avulla muodostamaan satunnaisia kokonaislukuja. Anna seuraavassa esimerkissä parametreiksi arvoalue, jolta satunnainen luku halutaan generoida, sekä kokonaislukumuuttuja:

```
Elm
let
    valueRange = (1,10)
    randomNumber = Random.generateInt valueRange myInt
in
    { random: randomNumber }

```

Seuraava koodi esittää, kuinka voidaan generoida satunnaisia desimaalilukuja käyttämällä `Random`-moduulia ja `generateFloat`-funktiota:

```
Elm
let
    valueRange = (1,10)
    randomNumber = Random.generateFloat valueRange 1.0
in
    { random: randomNumber }

```

Tässä esimerkissä arvoalueeksi on määritetty 1-10 ja desimaaliluku generoidaan satunnaisesti desimaalinumeron jälkeen.

## Syvemmälle satunnaisiin lukuihin Elm-kielellä

Elm-kielellä satunnaisia lukuja generoidaan seedien avulla, jotka ovat satunnaisuuden lähtökohtia. Seedit annetaan `Random.seed`-funktiolle ja niitä voi uudelleenkäyttää aina, kun tarvitaan uusia satunnaisia lukuja samasta lähtökohtasta. Satunnaisuutta voi muokata myös käyttämällä muita `Random`-moduulin funktioita, kuten `map` tai `andThen`.

## Katso myös

- Ohjeet ja dokumentaatio satunnaislukujen generoimiseen Elm-kielellä: https://package.elm-lang.org/packages/elm/random/latest/
- Esimerkkejä satunnaislukujen käytöstä Elm-koodissa: https://elmprogramming.com/generate-random-number-elm.html