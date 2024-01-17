---
title:                "Sanojen väliinpistäminen"
html_title:           "Gleam: Sanojen väliinpistäminen"
simple_title:         "Sanojen väliinpistäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringin interpolointi tarkoittaa muuttuvan arvon sisällyttämistä merkkijonoon. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan tulostaa muuttuja arvojen kanssa tekstiä. Ohjelmoijat käyttävät sitä laajasti tekstinmuodostuksessa ja -käsittelyssä.

## Kuinka: 
### Esimerkki 1:
```
Gleam.run( str"Hello, I am #{name}" )
```
Output:
```
Hello, I am John
```

### Esimerkki 2:
```
let language = "Gleam"

Gleam.run( str"Welcome to the #{language} programming language!" )
```
Output:
```
Welcome to the Gleam programming language!
```

## Syvemmälle:
Interpoloinnin idea on peräisin Perl-ohjelmointikielestä, mutta nykyään se on yleinen ominaisuus monissa ohjelmointikielissä, kuten Rubyssa ja JavaScriptissä. Toisinaan sitä kutsutaan myös nimellä string templating. Käytännössä interpoloinnissa käytettävä muuttuja- tai arvo lista lisätään merkkijonon sisälle halutulla tavalla, kun sitä koodissa suoritetaan. Gleamissa tämä tapahtuu ```#{arvojen_listassa}```-muodolla.

## Katso myös:
- Gleam-ohjelmointikielen virallinen dokumentaatio: https://gleam.run/
- Kattava opas Gleam-nimiselle kielenä: [linkki]