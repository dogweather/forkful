---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:34.848920-07:00
description: "Kuinka: Google Apps Scriptiss\xE4 assosiatiivisia taulukoita (objekteja)\
  \ luodaan ja k\xE4sitell\xE4\xE4n k\xE4ytt\xE4m\xE4ll\xE4 aaltosulkeita `{}`, m\xE4\
  \xE4rittelem\xE4ll\xE4 avain-arvo\u2026"
lastmod: '2024-03-13T22:44:56.088733-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Scriptiss\xE4 assosiatiivisia taulukoita (objekteja) luodaan\
  \ ja k\xE4sitell\xE4\xE4n k\xE4ytt\xE4m\xE4ll\xE4 aaltosulkeita `{}`, m\xE4\xE4\
  rittelem\xE4ll\xE4 avain-arvo -pareja sis\xE4ll\xE4."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
Google Apps Scriptissä assosiatiivisia taulukoita (objekteja) luodaan ja käsitellään käyttämällä aaltosulkeita `{}`, määrittelemällä avain-arvo -pareja sisällä. Avaimet ovat uniikkeja tunnisteita, ja arvot voivat olla mitä tahansa merkkijonoista ja numeroista monimutkaisempiin objekteihin tai funktioihin. Tässä on perusesimerkki:

```javascript
function createAssociativeArray() {
  var user = {
    name: "John Doe",
    age: 30,
    email: "johndoe@example.com"
  };

  // Arvojen käyttö
  Logger.log(user.name); // Tulostaa: John Doe
  Logger.log(user["email"]); // Tulostaa: johndoe@example.com

  // Uusien avain-arvo -parien lisääminen
  user.title = "Ohjelmistokehittäjä";
  user["country"] = "USA";

  Logger.log(user.title); // Tulostaa: Ohjelmistokehittäjä

  // Iterointi avain-arvo -pareilla
  for (var key in user) {
    Logger.log(key + ': ' + user[key]);
  }
}
```

Iteroinnin osalta näyteulostus saattaisi näyttää tältä:
```
name: John Doe
age: 30
email: johndoe@example.com
title: Ohjelmistokehittäjä
country: USA
```

Huomaa, kuinka voit käyttää sekä pistenotaatiota että hakasulkenotaatiota ominaisuuksien käyttämiseen ja asettamiseen. Hakasulkenotaatio on erityisen hyödyllinen työskenneltäessä dynaamisesti määräytyvien avainten kanssa tai avaimien kanssa, jotka sisältävät tunnisteissa sallimattomia merkkejä.

## Syväsukellus
Objekteina toimivat assosiatiiviset taulukot ovat olleet JavaScriptin, ja laajennettuna Google Apps Scriptin, kulmakivi, heijastellen sen prototyyppiperusteista perintämekanismia. Toisin kuin perinteiset assosiatiiviset taulukot tai sanakirjat tarjoavat kielet (esim. Pythonin dict), Google Apps Script -objektit tarjoavat joustavan ja tehokkaan tavan rakentaa tietoja, hyötyen JavaScriptin dynaamisesta luonteesta.

On kuitenkin tärkeää huomata, että ECMAScript 2015 -määrittely toi mukanaan `Map`- ja `Set`-objektit, jotka tarjoavat suoraviivaisemman assosiatiivisen kokoelmien käsittelyn tietyin etuin objekteihin verrattuna, kuten säilyttäen lisäysjärjestyksen ja paremman suorituskyvyn suurille tietomäärille. Vaikka Google Apps Script tukee näitä myös, valinta objektien tai uudempien `Map`/`Set`-rakenteiden välillä riippuu erityisistä tarpeista ja suorituskykyharkinnoista. Useimmille assosiatiivista taulukkoa koskeville tehtäville perinteiset objektipohjaiset toteutukset tarjoavat tutun ja monipuolisen lähestymistavan, mutta uudempien vaihtoehtojen tutkiminen on suositeltavaa käsikirjoituksesi monimutkaisuuden kasvaessa.
