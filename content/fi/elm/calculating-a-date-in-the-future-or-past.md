---
title:    "Elm: Tulevaisuuden tai menneen päivämäärän laskeminen"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi laskea tulevaisuuden tai menneen päivän? Elm-kielinen päivämäärälaskenta voi auttaa sinua muotoilemaan ja näyttämään päivämääriä eri tavoin. Esimerkiksi voit lisätä sivustollesi ominaisuuden, joka näyttää käyttäjän syntymäpäivän viikonpäivän tai lasketun viimeisen maksupäivän.

## Miten

```Elm
import Date

Date.fromIsoString "2021-04-20" -- saa takaisin 20. huhtikuuta 2021
Date.fromParts 2021 April 20 -- saa takaisin 20. huhtikuuta 2021
```

Voit myös lisätä päiviä, kuukausia tai vuosia olemassa olevaan päivämäärään:

```Elm
Date.add Days 5 (Date.fromIsoString "2021-04-20") -- saa takaisin 25. huhtikuuta 2021
Date.add Months 2 (Date.fromIsoString "2021-04-20") -- saa takaisin 20. kesäkuuta 2021
Date.add Years (-3) (Date.fromIsoString "2021-04-20") -- saa takaisin 20. huhtikuuta 2018
```

Voit myös tarkistaa, onko päivämäärä ennen vai jälkeen toisen päivämäärän:

```Elm
Date.isBefore (Date.fromIsoString "2021-04-20") (Date.fromIsoString "2021-04-21") -- saa takaisin True
Date.isAfter (Date.fromIsoString "2021-04-25") (Date.fromIsoString "2021-04-20") -- saa takaisin True
```

## Syventävä sukellus

Elm-kielinen päivämäärälaskenta käyttää sisäisesti JavaScriptin `Date`-objektia, joten se tukee samoja toimintoja kuin JavaScriptissa. Voit tutkia tarkemmin `Date`-objektin dokumentaatiota löytääksesi muita käyttökelpoisia toimintoja päivämäärälaskentaan.

## Katso myös

- [Elm-kielen virallinen dokumentaatio](https://guide.elm-lang.org/)
- [JavaScriptin `Date`-objektin dokumentaatio](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)