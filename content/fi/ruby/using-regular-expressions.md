---
title:    "Ruby: Säännöllisten ilmaisujen käyttäminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Regular expressionit ovat hyödyllisiä työkaluja, jotka auttavat muuttamaan tekstiä ja löytämään tiettyjä osioita tiedostoista tai merkkijonoista. Niitä käytetään usein ohjelmoinnissa datan käsittelyssä ja muokkaamisessa.

## Kuinka käyttää

Regular expressionit ovat osa Ruby-koodia ja ne kirjoitetaan käyttämällä tiettyjä merkintöjä. Esimerkiksi, jos haluat löytää kaikki sanat, jotka alkavat kirjaimella "a", voit käyttää seuraavaa koodia:

```Ruby
string = "apple banana avocado"
matches = string.scan(/\ba\w+/)
puts matches
```

Tämä koodi tulostaa ["apple", "avocado"].

## Syvempää tietoa

Regular expressioneilla on omat sääntönsä ja merkinnät, ja niiden oppiminen vaatii harjoittelua ja kärsivällisyyttä. Hyvä tapa oppia niitä on kokeilla erilaisia koodinpätkiä ja katsoa miten ne vaikuttavat erilaisiin merkkijonoihin. Myös säännöllinen ilmaisnäyttö voi auttaa hahmottamaan niitä paremmin.

## Katso myös

- [Ruby Regular Expressions Guide](https://www.rubyguides.com/2015/06/ruby-regex)
- [Rubular - Regular Expression Tester](https://rubular.com)
- [Regex101 - Regular Expression Testing Tool](https://regex101.com/)