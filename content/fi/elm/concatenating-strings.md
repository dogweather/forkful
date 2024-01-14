---
title:                "Elm: Merkkijonojen yhdistäminen"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Usein tapahtuu, että ohjelmointiprojektissa halutaan yhdistää kaksi tai useampia merkkijonoja yhdeksi kokonaiseksi merkkijonoksi. Tätä kutsutaan merkkijonojen konkaternoinniksi ja se on tärkeä osa monia Elm-ohjelmia.

## Kuinka

Merkkijonojen konkaternointi Elm-ohjelmassa voidaan tehdä käyttäen `++` operaattoria. Se ottaa kaksi merkkijonoa ja yhdistää ne yhdeksi merkkijonoksi. Esimerkiksi:

```elm
"Hello, " ++ "World" 
```

tuottaa tuloksen `Hello, World`.

Merkkijonojen konkaternointi on hyödyllistä, kun halutaan luoda dynaamisia viestejä tai generoida syötteitä käyttäjän antamien tietojen perusteella. Se on myös hyödyllistä, kun halutaan rakentaa dynaamisia otsikoita tai aloitusviestejä sovelluksessa.

## Syväsukellus

Elm tarjoaa myös muita tapoja konkaternoida merkkijonoja, kuten `concat` funktiota, joka ottaa listan merkkijonoja ja yhdistää ne yhdeksi merkkijonoksi. Tämä on kätevä silloin, kun halutaan käsitellä suurempia määriä merkkijonoja. Esimerkiksi:

```elm
["Hello", ", ", "World"] 
|> concat
```

tuottaa saman tuloksen kuin edellinen esimerkki.

On myös mahdollista käyttää [String Builder](https://package.elm-lang.org/packages/elm/string-builder/latest/) kirjastoa, joka tarjoaa erilaisia työkaluja merkkijonojen hallintaan, mukaan lukien konkaternointi.

## Katso myös

- [Elm: A delightful language for reliable webapps](https://elm-lang.org/)
- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm String Builder Documentation](https://package.elm-lang.org/packages/elm/string-builder/latest/StringBuilder)