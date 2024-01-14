---
title:                "Gleam: Säännöllisen ilmaisujen käyttö"
simple_title:         "Säännöllisen ilmaisujen käyttö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi

Kun ohjelmointia tehdään Gleamilla, monissa tilanteissa tarvitaan joukko toimintoja, joilla voidaan käsitellä merkkijonoja ja suorittaa hakuja ja muutoksia niissä. Tämä on silloin, kun regular expression (säännölliset lausekkeet) astuvat kuvaan.

## Kuinka käyttää

Regular expressionin käyttäminen Gleamilla on helppoa ja tehokasta. Alla on esimerkki funktiosta, joka etsii annetussa merkkijonossa kaikki numerot ja palauttaa ne listana:

```Gleam
fn find_numbers(str) {
  r: Regex.from_str("\\d+")
  matches: Regex.find_all(r, str)
  List.map(matches, { match | String.to_int(match) })
}
```

Tässä esimerkissä käytetään Regex-moduulia, joka tarjoaa joukon toimintoja regular expressionien luomiseen ja käyttämiseen. Funktio `Regex.find_all` etsii annetusta merkkijonosta kaikki kohtaamaista regular expressionia vastaavat osat ja palauttaa ne listana. Tämän jälkeen `List.map`-funktio käy läpi listan ja muuttaa jokaisen kohteen merkkijonosta kokonaisluvuksi `String.to_int`-funktion avulla.

Yllä olevan funktion käyttö näyttäisi tältä:

```Gleam
numbers: find_numbers("Osoitteestani löytyy 123 456 kaupunkia.")
// numbers = [123, 456]
```

Gleam tarjoaa myös muita käteviä toimintoja, kuten `Regex.replace`, jolla voidaan korvata annetussa merkkijonossa olevat osat uudella tekstillä. Voit lukea lisää regular expressionien käytöstä Gleamilla [Gleamin virallisesta dokumentaatiosta](https://gleam.run/documentation).

## Syväsukellus

Regular expressionit voivat vaikuttaa aluksi hankalilta ymmärtää, mutta niiden käyttö on todella hyödyllistä ja tehokasta ohjelmoinnissa. Niiden avulla voidaan suorittaa monimutkaisiakin hakuja ja muutoksia merkkijonoissa ja niiden avulla säästetään paljon aikaa ja vaivaa.

Muutamia hyödyllisiä vinkkejä ja tarkennuksia regular expressionien käyttöön:

- Regular expressioneja voidaan käyttää myös `String`-moduulin funktioissa, kuten `String.contains` ja `String.split`.
- Gleamissa regular expressionien käytössä kannattaa käyttää `\`-merkkiä escape-sekvenssinä, joka viittaa merkkiin, joka tarvitsee erityiskohtelua, kuten `+` tai `.`.
- Gleamissa regular expressionit käsitellään `String`-tyyppisinä, mutta muista muuttaa halutut osat `String`-tyypiksi ennen niiden käyttöä.

## Katso myös

- [Gleamin virallinen dokumentaatio](https://glean.run/documentation)
- [Regex-moduulin dokumentaatio](https://gleam.run/modules/regex/latest/Regex.html)