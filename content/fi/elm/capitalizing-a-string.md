---
title:    "Elm: Merkkijonon kirjoitustapaa muuttaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi muuttaa merkkijonon kirjainkoon suuremmaksi? Elm-ohjelmointikielen avulla tämä on helppoa ja nopeaa toteuttaa.

## Miten tehdä

Käytännönläheinen koodiesimerkki osoittaa, kuinka yksinkertaisesti voit muuttaa merkkijonon kirjainkoon Elmissä:

```Elm 
capitalizedString = String.toUpper "kirjoitettu pienillä"
```

Tämä koodi muuttaa merkkijonon "kirjoitettu pienillä" kaikki kirjaimet isoiksi ja tallentaa ne uuteen muuttujaan "capitalizedString". Voit myös muuttaa vain yhden kirjaimen kerrallaan käyttämällä `String.toUpper` funktiota ja antamalla sen parametrina kyseisen kirjaimen.

## Syvemmät tiedot

Elm tarjoaa useita erilaisia toimintoja merkkijonon käsittelyyn, mukaan lukien myös muutokseen kirjainkoossa. Voit esimerkiksi yhdistää useita merkkijonoja yhteen käyttämällä `++` operaattoria, tai etsiä tietyn merkkijonon osan toisesta merkkijonosta käyttämällä `String.contains` funktiota.

Elmissä merkkijonojen käsittely on nopeaa ja tehokasta, mutta on myös tärkeää pitää mielessä, että kaikki merkkijonot ovat kiinteitä arvoja eivätkä muutu suorituksen aikana. Näin ollen esimerkiksi muuttaessa merkkijonon kirjainkoko suuremmaksi, uusi merkkijono luodaan sen sijaan, että alkuperäistä muutettaisiin.

## Katso myös

- [Elm dokumentaatio merkkijonojen käsittelystä](https://guide.elm-lang.org/strings/)
- [Muita hyödyllisiä ohjelmointikieliä suomen kielellä](https://www.codingame.com/platforms/codingame-rust/)