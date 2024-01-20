---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# JavaScriptin Nykyisen Päivämäärän Haku

## Mitä & Miksi?

Nykyisen päivämäärän hakeminen tarkoittaa tämänhetkisen päivämäärän ja kellonajan selvittämistä ohjelmoimalla. Se on välttämätöntä ajanherkkien tehtävien, kuten aikaleimojen, aikataulutuksen ja päivämäärärajoitettujen ominaisuuksien, hallitsemiseksi.

## Näin teet:

Käytämme `Date`-luokkaa nykyisen päivämäärän saamiseksi JavaScriptissa.

```Javascript
let päivämäärä = new Date();
console.log(päivämäärä);
```

Tämä koodipätkä tulostaa jotakin seuraavanlaista:

```Javascript
2022-05-28T12:34:56.789Z
```

## Syvä sukellus

Historiallisesti JavaScriptin `Date`-luokka on ollut perustana kaikille päivämäärä- ja aikatoiminnoille siitä lähtien, kun se lisättiin kielen spesifikaatioon. Se käyttää vuodesta 1970 lähtevää Unix-aikaa sisäisenä pohjanaan - arkkitehtoninen päätös, joka heijastaa JavaScriptin juuria web-selausympäristönä.

Vaihtoehtoisia päivämääräkirjastoja, kuten `Moment.js` ja `date-fns`, käytetään usein parantamaan päivämäärätoimintojen käyttöä selkeämmällä syntaksilla ja lisäominaisuuksilla. Kuitenkin suurimmalle osalle sovelluksia `Date`-luokka tarjoaa riittävät työkalut.

JavaScriptin `Date`-objekti ei ota huomioon aikavyöhykkeitä luodessaan uuden instanssin. Sitä voidaan käyttää aikavyöhyke-spesifisten juttujen tekemiseen ajamalla aika UTC-muotoon.

## Katso myös 

- MDN Web Docs, [JavaScript Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js, [Moment.js documentation](https://momentjs.com/)
- date-fns, [date-fns documentation](https://date-fns.org/docs/Getting-Started)