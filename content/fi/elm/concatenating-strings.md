---
title:    "Elm: Merkkijonojen yhdistäminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Elm-ohjelmointikielessä merkkijonojen yhdistäminen eli konkatenointi on erittäin kätevä toiminto, joka mahdollistaa tekstien ja muuttujien yhdistämisen yhdeksi merkkijonoksi. Sillä voidaan esimerkiksi helposti luoda dynaamisia otsikkoja tai viestejä, jotka sisältävät muuttuvia arvoja.

## Miten

```Elm
concatenate : String -> String -> String
concatenate a b =
    a ++ b
```

```Elm
name = "Ella"
greeting = "Hei " ++ name ++ ", tervetuloa Elm-ohjelmointimaailmaan!"
```

**Output:**

```
Hei Ella, tervetuloa Elm-ohjelmointimaailmaan!
```

## Syvempi sukellus

Elm:ssä merkkijonojen konkatenointi tapahtuu `++`-operaattorin avulla, joka yhdistää kaksi merkkijonoa yhdeksi. Itse asiassa `++` ei toimi pelkästään merkkijonoilla, vaan se pystyy yhdistämään myös listoja, joten sitä voidaan käyttää myös monipuolisemmin. On myös hyvä huomioida, että merkkijonojen yhdistäminen ei ole tehokkain tapa käsitellä suuria määriä dataa, sillä jokainen konkatenointi luo uuden merkkijonon, joka vie muistia.

## Katso myös

- [Elm-ohjelmointikielen virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Elm-ohjelmointikielen opiskeluresurssit](https://elm-lang.org/resources)
- [Elm-ohjelmointikielen virallinen Slack-kanava](https://elmlang.slack.com/)