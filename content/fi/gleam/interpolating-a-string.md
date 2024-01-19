---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Merkkijonojen interpolointi on ohjelmointikielestä riippumaton tekniikka, jossa ohjelma luo uuden merkkijonon arvot, jotka on määritetty muuttujissa tai lausekkeissa, sisällyttämällä suoraan merkkijonoon. Koodarit käyttävät sitä koodin legibiliteetin parantamiseksi ja toistuvan koodin vähentämiseksi.

## Näin teet:

Tässä on esimerkki Gleam-koodista.

```Gleam
let nimi = "Koodari"
let tervehdys = "Hei, {nimi}!"
io.println(tervehdys)
```

Lopputulos olisi:

```
"Hei, Koodari!"
```

## Syvä sukellus:

Merkkijonojen interpolointi ei ole uusi idea, se on ollut sellaisissa kielissä kuin Perl 1980-luvulta asti. Gleamissa, toisin kuin joissakin kielissä, merkkijonojen interpolointi suoritetaan soveltamalla syntaksin sokeria, eikä se ole kielen ominaisuus.

Vaihtoehtoisesti, voit käyttää merkkijonon yhdistämistä saavuttaaksesi saman tuloksen, mutta se ei ole yhtä selkeä ja on altis virheet:

```Gleam
let nimi = "Koodari"
let tervehdys = "Hei, " ++ nimi ++ "!"
io.println(tervehdys)
```

Implementointi yksityiskohtiin sukeltaminen, Gleam korvaa interpoloidut lausekkeet niiden arvoilla suoritusaikana.

## Katso myös:

- Gleam `String` dokumentaatio: https://hexdocs.pm/gleam_stdlib/gleam/string
- Wikipedia artikkeli merkkijonojen interpoloinnista: https://fi.wikipedia.org/wiki/Merkkijonon_interpolointi
- Stack Overflow keskustelu: https://stackoverflow.com/questions/tagged/string-interpolation