---
title:                "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisimme yhdistää merkkijonoja? Syy on yksinkertainen: se on tärkeä osa ohjelmointia ja se auttaa tekemään koodistamme tehokkaampaa.

## Miten

Aloita luomalla kaksi muuttujaa, joissa on erilaiset merkkijonot. Voit käyttää esimerkkinä etunimiä ja sukunimiä.

```
Gleam
```

Voimme yhdistää nämä kaksi merkkijonoa yhteen käyttämällä `<>` -operaattoria.

```
Gleam <> Programming
```

Tämä tuottaa tuloksen "Gleam Programming".

Voit myös yhdistää useampia merkkijonoja samassa lauseessa.

```
"Hello" <> " " <> "World"
```

Tämä tuottaa tuloksen "Hello World".

Voit myös käyttää `String.concat` -funktiota yhdistämään useampia merkkijonoja yhteen. Esimerkiksi:

```
String.concat(["Gleam", " ", "Programming"])
```

Tämä tuottaa saman tuloksen kuin edellinen esimerkki.

## Syväsukellus

Miksi käyttäisimme `<>` -operaattoria tai `String.concat` -funktiota sen sijaan, että kirjoittaisimme yhteenketjuvan merkkijonon suoraan? Yksi syy on, että se helpottaa muuttujien käyttöä ja lisää koodimme luettavuutta. Lisäksi se sallii dynaamisen yhdistämisen, eli voimme käyttää muuttujia ja laskutoimituksia merkkijonojen yhdistämisessä.

Merkkijonojen yhdistäminen on myös olennainen osa monimutkaisempia ohjelmia, kuten tietokantakyselyiden tai web-sovellusten tekemistä.

## Katso myös

- [Gleam-kielen dokumentaatio](https://gleam.run)
- [Merkkijonojen yhdistäminen Ruby-kielessä](https://ruby-doc.org/core-2.7.1/String.html#method-i-2B)
- [Merkkijonojen yhdistäminen JavaScript-kielessä](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/concat)