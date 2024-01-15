---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Elm: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi muuttaa merkkijono pieniksi kirjaimiksi? Yksinkertaistettuna: se voi olla kätevää tietojen käsittelyssä. Esimerkiksi vertaillessa kahta merkkijonoa, halutaan varmistua siitä, että molemmat ovat pienillä kirjaimilla kirjoitettuja, jotta vertailu on oikeaoppinen.

## Kuinka

Oletetaan, että haluat kokonaan muuttaa merkkijonon sisältämät kirjaimet pieniksi kirjaimiksi. Elm:n sisäänrakennettu funktio `String.toLower` tekee tämän puolestasi. Alla on esimerkki koodista ja sen tuottama tuloste tietyillä syötteillä:

```Elm
String.toLower "HELLO" --> "hello"
String.toLower "EVEryDay" --> "everyday"
String.toLower "123ABC" --> "123abc"
```

Funktio toimii myös merkkijonoissa, jotka sisältävät erikoismerkkejä tai suomen kielen aakkosia:

```Elm
String.toLower "ÄITI" --> "äiti"
String.toLower "hYvÃ¤ kaIkeN kaUkkeuSsA" --> "hyvä kaikeen kaukkeudessa"
```

## Deep Dive

Vaikka `String.toLower` on kätevä funktio, se ei ole aina sopiva ratkaisu. Esimerkiksi jos merkkijonossa on mukana myös numeeroita tai erikoismerkkejä, ne jäävät muuttumattomiksi. Tämä voi aiheuttaa ongelmia halutessa tarkistaa koko merkkijonon pienet kirjaimet.

Toinen huomioitava seikka on, että `String.toLower` toimii vain standardilla ASCII-merkkikoodilla. Jos tarvitset konversiota muille koodauksille, kuten UTF-8 tai ISO-8859-1, tarvitaan lisää funktioita tai kirjastoja.

## Katso myös

- [String API reference](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm dokumentaatio](https://guide.elm-lang.org/)
- [Elm Slack -yhteisö](https://elmlang.slack.com/)