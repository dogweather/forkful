---
title:    "Elm: Päivämäärän saaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi: 
Elm on ohjelmointikieli, joka on suunniteltu helpottamaan käyttäjän elämää. Yksi tapa tehdä tämä on saada ajankohtaista tietoa, kuten nykyinen päivämäärä, helposti ja tarkasti, ja Elm tarjoaa erinomaiset työkalut tähän tarpeeseen.

## Miten: 
Käyttämällä Elm Date -moduulia, voit helposti saada nykyisen päivämäärän koodisi sisällä. Katso alla oleva esimerkki:
```
Elm Day muodostaa
  title : Date
  title =
    Date.today
```
Tämä esimerkki luo muuttujan "title" ja asettaa sen nykyiseen päivämäärään.

Voit myös mukauttaa päivämäärämuotoa käyttämällä funktioita, kuten "Date.month", "Date.day" ja "Date.year" saadaksesi tietyn osan päivämäärästä. Alla olevassa esimerkissä käytetään "Date.day" -funktiota saadaksesi nykyisen kuukauden päivämäärän:
```
Elm Day muodostaa
  title : Int
  title =
    Date.day Date.today
```
Tämä tulostaisi numeron nykyisestä päivästä, joka on muuttujassa "title".

## Syvällinen perehtyminen:
Kun käsittelemme päivämäärän saamista, tärkeää on ymmärtää aikavyöhykkeet ja niiden vaikutus. Elm Date -moduuli käyttää standardia UTC-aikavyöhykettä, joten jos haluat saada päivämäärän tietyssä aikavyöhykkeessä, sinun on käytettävä funktiota "Date.fromZone" ja annettava aikavyöhykkeen nimi.

On myös tärkeää muistaa päivämäärän muotoilu ja lokalisointi. Elm Date -moduuli tarjoaa funktioita, kuten "Date.toText", jotta voit muuntaa päivämäärän haluamaasi muotoon tai kielelle.

## Katso myös: 
- Elm Date -moduulin virallinen dokumentaatio: [linkki](https://package.elm-lang.org/packages/elm/time/latest/Date)
- Lisää tietoa aikavyöhykkeistä: [linkki](https://fi.wikipedia.org/wiki/Aikavy%C3%B6hyke)
- Ohjeita päivämäärän muotoilemiseen: [linkki](https://pypi.org/project/dtf/)