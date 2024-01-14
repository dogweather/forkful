---
title:    "Elm: Merkkijonon muuttaminen pieniksi kirjaimiksi"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi: Miksi haluaisit muuntaa merkkijonon pieniksi kirjaimiksi

Joskus ohjelmoinnissa joudut käsittelemään merkkijonoja, ja usein näiden merkkijonojen takaisin olevat kirjaimet eivät ole tärkeitä. Tällöin voi olla hyödyllistä muuntaa ne pieniksi kirjaimiksi, jotta työtä varten tarvitsemasi tiedot ovat helpommin luettavissa ja käsiteltävissä.

## Miten: Esimerkkejä ja koodinpätkiä Elmille konvertoidessa merkkijonoja pieniksi kirjaimiksi

### Koodinpätkät

```Elm
toLower "ELM ON UPEA OHJELMISTOKIELI" -- Output: "elm on upea ohjelmistokieli"
```
```Elm
String.toLower "ELM ON UPEA OHJELMISTOKIELI" -- Output: "elm on upea ohjelmistokieli"
```
```Elm
String.toLowercase "ELM ON UPEA OHJELMISTOKIELI" -- Output: "elm on upea ohjelmistokieli"
```

```Elm
String.toLower "KOODAAMINEN ON HAUSKAA!" -- Output: "koodaaminen on hauskaa!"
```
```Elm
String.toLowercase "KOODAAMINEN ON HAUSKAA!" -- Output: "koodaaminen on hauskaa!"
```

### Selitys

Koodinpätkissä käytämme `String.toLower`-funktiota, joka muuntaa annetun merkkijonon pieniksi kirjaimiksi. Voimme myös käyttää `String.toLowercase`-funktiota, jolla on sama vaikutus.

## Syväluotaus: Tarkempaa tietoa merkkijonon muuntamisesta pieniksi kirjaimiksi

Merkkijonon muuntaminen pieniksi kirjaimiksi on hyödyllistä, jos esimerkiksi haluat vertailla kahta merkkijonoa, mutta haluat tehdä sen ilman että isot ja pienet kirjaimet vaikuttavat vertailuun. Voit myös muuntaa käyttäjän antaman syötteen pienenä kirjainmuotoon, jolloin käsiteltävänä olevat tiedot ovat yhtenäisesti pieninä kirjaimina.

## Katso myös

- [String.toUpper](https://package.elm-lang.org/packages/elm/core/latest/String#toUpper)
- [String.capitalize](https://package.elm-lang.org/packages/elm/core/latest/String#capitalize)