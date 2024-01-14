---
title:                "Javascript: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat hyödyllisiä työkaluja, kun haluat käsitellä ja etsiä tietoa tekstistä. Ne ovat erityisen käteviä, kun haluat löytää tietyn muotoisia sanoja tai ilmaisuja suuresta tekstimäärästä, kuten esimerkiksi luettelosta tai asiakirjasta.

## Miten aloitat säännöllisten lausekkeiden käytön?

Säännöllisten lausekkeiden käyttö aloitetaan *regexp*-funktion avulla. Voit luoda uuden regexp-olion ja määrittää haluamasi hakuehdot sisään. Esimerkiksi voit etsiä kaikki sanat, jotka alkavat "a"-kirjaimella seuraavalla koodilla:

```Javascript
let regexp = new RegExp("^a\\w+"); // Hakee kaikki "a"-alkuiset sanat
```

Jos haluat etsiä useampia hakuehtoja, voit käyttää erilaisia säännöllisten lausekkeiden metodeja, kuten `.test()` tai `.match()`. Näillä metodeilla voit tarkistaa, täyttyvätkö hakuehdot tekstissä ja/tai palauttaa tulokset taulukkona tai muuna datana. Käytännön esimerkkejä löydät [täältä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions).

## Syvempi sukellus säännöllisiin lausekkeisiin

Säännöllisten lausekkeiden hallitseminen voi tuntua aluksi haastavalta, mutta niiden käytön oppiminen kannattaa. Voit saavuttaa säännöllisten lausekkeiden avulla monia asioita, kuten:

- Yksinkertaistaa tekstin käsittelyä ja tarkistamista
- Tehdä monimutkaisia muutoksia tekstiin
- Parsia tietoa taulukoihin tai objekteihin helposti

Hyvä tapa oppia säännöllisten lausekkeiden käyttöä on harjoitus: kokeile erilaisia hakuehtoja ja näet, miten ne vaikuttavat tuloksiin. Voit myös hyödyntää verkosta löytyviä oppaita ja oppimateriaaleja.

## Katso myös

- [MDN - Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [Kokonaisvaltainen opas säännöllisiin lausekkeisiin (englanniksi)](https://regexone.com/)
- [W3Schools - Regular Expressions](https://www.w3schools.com/js/js_regexp.asp)