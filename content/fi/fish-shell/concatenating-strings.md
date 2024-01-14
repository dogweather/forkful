---
title:    "Fish Shell: Merkkijonojen yhdistäminen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja Fish Shell -ohjelmoinnissa? On olemassa monia tilanteita, joissa sinun on luotava uusi merkkijono yhdistämällä olemassa olevia merkkijonoja. Tämä voi olla hyödyllistä esimerkiksi tekstianalyyseissä tai kun luot sisällönhallintajärjestelmiä.

## Miten

Fish Shellissa voit yhdistää merkkijonoja käyttämällä sanaa "echo" ja kahden merkkijonon peräkkäistä kirjoittamista. Esimerkiksi, jos haluat yhdistää merkkijonot "Terve" ja "päivää", kirjoitat seuraavan komennon:

```Fish Shell
echo "Terve" "päivää"
```

Tämän komennon suorittamisen jälkeen näet tuloksen "Terve päivää" konsolissasi. Voit myös yhdistää useampia merkkijonoja yhdellä komennolla yksinkertaisesti kirjoittamalla ne peräkkäin.

## Syvällisempi tutustum

Voit myös yhdistää merkkijonoja muuttujien kanssa käyttämällä "string concatenation" (merkkijonojen yhdistäminen). Tämä tarkoittaa yksinkertaisesti sitä, että sijoitat "+" -merkin kahden merkkijonon välille ja ne yhdistetään. Esimerkiksi, jos haluat luoda muuttujan nimeltä "nimi" arvolla "John" ja yhdistää sen merkkijonon "Hei", kirjoitat seuraavan komennon:

```Fish Shell
nimi="John"
echo "Hei " + $nimi
```

Tämä tulostaa "Hei John". On tärkeää huomata, että muuttujan nimi on kirjoitettu "$"-merkillä komennossa, jotta Fish Shell tietää, että sen sisältö tulee yhdistää merkkijonon kanssa.

## Katso myös

- Fish Shell virallinen verkkosivusto: https://fishshell.com/
- Fish Shell dokumentaatio: https://fishshell.com/docs/current/index.html
- Fish Shell yhteisö: https://github.com/fish-shell/fish-shell/