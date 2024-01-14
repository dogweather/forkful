---
title:                "Fish Shell: Normaalien ilmaisujen käyttäminen"
simple_title:         "Normaalien ilmaisujen käyttäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat erittäin hyödyllisiä työkaluja ohjelmoinnissa ja tiedonkäsittelyssä. Niitä voidaan käyttää muun muassa merkkijonojen etsimiseen, korvaamiseen ja muokkaamiseen. Ne säästävät aikaa ja vaivaa, kun käsitellään suuria määriä dataa.

## Kuinka käyttää säännöllisiä lausekkeita Fish Shellissa?

Fish Shellissa säännölliset lausekkeet ovat käytettävissä sisäänrakennettujen `string` -funktioiden avulla. Ne voidaan suorittaa seuraavassa muodossa:

```Fish Shell
string match --regex "hakulauseke" "teksti"
```
Tämä komento etsii annetusta tekstistä kaikki kohdat, jotka vastaavat annettua hakulauseketta. Voit myös käyttää `string replace` -komennon avulla korvata löydetyt kohdat toisella merkkijonolla.

```Fish Shell
string replace --regex "hakulauseke" "korvaava" "teksti"
```
Voit myös käyttää säännöllisiä lausekkeita muokkaamaan merkkijonoja `string sub` -komennolla. Esimerkiksi voit poistaa kaikki numerot annetusta tekstistä seuraavalla komennolla:

```Fish Shell
string sub -g -r '' '[0-9]' "teksti"
```

## Syvä sukellus säännöllisiin lausekkeisiin

Säännöllisten lausekkeiden avulla voit suorittaa monimutkaisia hakuja ja muokata dataa tarkasti. Ne ovat hyvin joustavia ja voit käyttää erilaisia metakaraktereita ja ilmaisuja hakulausekkeissa. Esimerkiksi `.` vastaa mihin tahansa yksittäiseen merkkiin ja `+` ilmaisee, että edellinen merkki esiintyy vähintään kerran. Voit myös käyttää hakulausekkeessa sana- tai lausekerajoja `(^)` ja `(?)` avulla.

Säännöllisissä lausekkeissa on myös erilaisia vaihtoehtoja, joiden avulla voit määrittää haun tarkkuuden. Voit esimerkiksi käyttää `(?i)` -vaihtoehtoa, joka tekee isojen ja pienten kirjainten erottelusta epätarkkaa.

## Katso myös

- [Fish Shellin dokumentaatio säännöllisille lausekkeille](https://fishshell.com/docs/2.3/cmds/string.html#string--match)
- [Lyhyt opas säännöllisiin lausekkeisiin](https://www.regular-expressions.info/quickstart.html)
- [Regex101 - Interaktiivinen työkalu säännöllisten lausekkeiden harjoitteluun](https://regex101.com/)