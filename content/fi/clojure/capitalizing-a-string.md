---
title:    "Clojure: Merkkijonon muuttaminen isoiksi kirjaimiksi"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon kirjainkoon?

## Miten tehdä

Merkkijonon kirjainkoon muuttamiseen on monia tapoja Clojure-ohjelmointikielessä. Yksi yksinkertainen tapa on käyttää "clojure.string/capitalize" -funktiota. Tämä muuttaa alkukirjaimen isoksi ja loput pieniksi kirjaimiksi.

```Clojure
(clojure.string/capitalize "tämä on esimerkki") ; "Tämä on esimerkki"
```

Toinen vaihtoehto on käyttää "clojure.string/upper-case" -funktiota, joka muuttaa kaikki kirjaimet isommiksi.

```Clojure
(clojure.string/upper-case "tämä on esimerkki") ; "TÄMÄ ON ESIMERKKI"
```

Nämä kaksi funktiota ovat hyvä vaihtoehto, jos haluat muuttaa merkkijonon kirjainkoon vain yhdestä syystä, kuten tulostuksen yhteydessä.

## Syvällisempi sukellus

Clojuressa merkkijonon kirjainkoon muuttamiselle on monia muitakin vaihtoehtoja, kuten "reduce" -funktio tai "map" -funktio yhdistettynä "char" -funktioon. Näiden vaihtoehtojen hyödyntäminen voi olla hyödyllistä, jos haluat muuttaa merkkijonon kirjainkoon useamman operaation yhteydessä.

Lisäksi Clojuren ydinbiblioteekissa on myös "clojure.string/lower-case" -funktio, joka muuttaa kaikki kirjaimet pieniksi.

## Katso myös

- "Clojure-järjestelmän riippuvuuksien hallinta - Johdanto" https://duunijs.com/clojure-jarjestelman-riippuvuuksien-hallinta/
- "Kuinka luoda web-sovellus Clojurella ja Liberatorilla" https://www.sitepoint.com/create-web-apps-clojure-liberator/
- "Clojure-kielen perusteet" http://www.cs.hut.fi/~ctl/Clojure-harjoituksia/Kierros-1/index.html