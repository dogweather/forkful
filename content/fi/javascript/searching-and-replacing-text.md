---
title:                "Tekstin hakeminen ja korvaaminen"
html_title:           "Javascript: Tekstin hakeminen ja korvaaminen"
simple_title:         "Tekstin hakeminen ja korvaaminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Hakukirjoittamisen ja tekstin korvaamisen tehtävänä on etsiä ja vaihtaa halutut osat tekstin sisällä. Tätä toimintoa käyttävät ohjelmoijat yleensä helpottaakseen ja nopeuttaakseen koodin muokkaamista sekä tehdäkseen massamuutoksia suuremmissa tekstimäärissä.

## Kuinka tehdä:
Esimerkiksi, jos haluat korvata kaikki sanat "vanha" tekstillä "uusi" seuraavassa lauseessa: "Minulla oli vanha puhelin", koodi näyttäisi tältä: 
```Javascript
lause = "Minulla oli vanha puhelin";
uusiLause = lause.replace("vanha", "uusi");
console.log(uusiLause); //Tuloste: "Minulla oli uusi puhelin"
```
Tässä koodin avulla käytetään Javascriptin sisäänrakennettua "replace" -funktiota tekstin korvaamiseen.

## Syväsukellus:
Hakukirjoittamisella ja tekstin korvaamisella on pitkä historia ohjelmoinnissa. Alkuaan niitä käytettiin varsinkin tekstinkäsittelyohjelmissa, mutta nykyään niitä käytetään laajasti myös ohjelmoinnissa. On myös olemassa muita vaihtoehtoja, kuten regular expression -kirjoittaminen, joka on hyödyllistä monimutkaisempien muutosten tekemisessä.

## Katso myös:
Linkkejä liittyviin lähteisiin:
- [Javascriptin sisäänrakennetun replace -funktion dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [Regular expression -kirjoittaminen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)