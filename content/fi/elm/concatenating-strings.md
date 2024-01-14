---
title:    "Elm: Merkkijonojen konkatenointi"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi

Miksi käyttäisit Elm-ohjelmointikieltä tietojen rinnakkain sijoittamiseen? Samoin kuin jokainen ohjelmointikieli, Elm tarjoaa tehokkaita työkaluja erilaisten tehtävien suorittamiseen. Yksi näistä tehtävistä on merkkijonojen yhdistäminen.

## Kuinka tehdä

Merkkijonojen yhdistäminen Elm-ohjelmointikielessä on hyvin yksinkertaista. Voit tehdä sen yksinkertaisesti käyttämällä sisäänrakennettua `++` operaattoria. Tarkastellaanpa esimerkkiä, jossa yhdistämme kaksi merkkijonoa:

```Elm
result: String
result =
    "Hei" ++ "maailma"
```

Tämä tuottaa tuloksen `"Heimaailma"`. Huomaa, että `++` operaattori yhdistää merkkijonot kirjaimellisesti ilman välilyöntejä. Jos haluat välilyönnin merkkijonojen välille, voit lisätä sen toisen merkkijonon loppuun ennen yhdistämistä.

## Syvemmällä

Merkkijonojen yhdistäminen ei ole vain yksinkertainen tapa luoda uusia merkkijonoja, se voi myös olla hyödyllinen työkalu muodostettaessa dynaamisia lausekkeita. Esimerkiksi, jos haluat luoda ilmoituksen käyttäjälle, voit käyttää `++` operaattoria lisäämällä dynaamisen muuttujan yhteen merkkijonon kanssa:

```Elm
user: String
user = "John"

message: String
message =
    "Tervetuloa " ++ user ++ "!"

-- Tulos: "Tervetuloa John!"
```

Tämä voi olla erityisen hyödyllinen, kun luodaan käyttöliittymää, joka näyttää muuttuvia tietoja. Merkkijonojen yhdistäminen mahdollistaa myös monimutkaisempien lausekkeiden muodostamisen muuttujien kanssa, kuten matemaattisten toimintojen ja ehtolauseiden yhdistämisen.

# Katso myös

- [Elm - Offical Website](https://elm-lang.org/)
- [Elm Introduction Documentation (in Finnish)](https://guide.elm-lang.org/architecture/)
- [Elm Slack Community (in Finnish)](https://elmlang.slack.com/archives/C0DEGJ2LP/)