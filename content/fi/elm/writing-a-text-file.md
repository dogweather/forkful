---
title:                "Elm: Tiedosto-ohjelman kirjoittaminen"
simple_title:         "Tiedosto-ohjelman kirjoittaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla tekstiä tiedostoon, voit tallentaa tietoa pysyvästi ja jakaa sitä muiden kanssa. Tekstin kirjoittaminen on myös yksi perustaidoista Elm-ohjelmoinnissa ja auttaa sinua ilmaisemaan ideoitasi ja luomaan toimivia sovelluksia.

## Miten tehdä

Elm on funktionaalinen ohjelmointikieli, joten tekstin kirjoittaminen tapahtuu funktioiden avulla. Käytä funktiota `writeFile` ja anna sille parametriksi haluamasi tiedostonimi ja sen sisältö, kuten esimerkissä:

```Elm
writeFile "teksti.txt" "Tämä on tekstiä"
```

Tämä luo tiedoston nimeltä "teksti.txt" ja tallentaa siihen tekstin "Tämä on tekstiä". Voit myös käyttää funktiota `appendFile` lisätäksesi tekstiä olemassa olevaan tiedostoon.

## Syväsukellus

Tekstin tallentaminen tiedostoon on tärkeä osa monia Elm-sovelluksia, kuten esimerkiksi blogin kirjoittamista tai käyttäjätilastojen tallentamista. Voit myös käyttää tekstieditointitoimintoa `Element.textarea` luodaksesi sovelluksia, joissa käyttäjät voivat kirjoittaa ja tallentaa tekstiä.

## Katso myös

- [Elm-kielen dokumentaatio](https://elm-lang.org/docs)
- [Tekstin tallentaminen tiedostoon Elmissä](https://guide.elm-lang.org/fundamentals/text.html)
- [Tekstieditointitoiminnon käyttäminen Elmissä](https://package.elm-lang.org/packages/elm/browser/latest/Browser#textarea)