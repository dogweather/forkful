---
title:                "Merkkijonojen yhdistäminen"
aliases:
- fi/ruby/concatenating-strings.md
date:                  2024-01-20T17:35:51.522305-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen yhdistäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? / Mikä & Miksi?
Yhdistämme merkkijonoja eli "stringejä" saumattomasti yhteen. Tähän on syynsä: näytetään käyttäjälle kustomoituja viestejä, rakennetaan dynaamista dataa tai muodostetaan koodin sisällä olevia hakulauseita.

## How to: / Kuinka:
Merkkijonojen yhdistäminen eli "concatenation" Rubyssa on suoraviivaista. Tässä pari tapaa:

```Ruby
# Plus-merkin käyttäminen
tervehdys = "Hei " + "maailma!"
puts tervehdys  # Tulostuu: Hei maailma!

# Selkeyden vuoksi käytetään "interpolationia"
nimi = "Matti"
viesti = "Moi, #{nimi}!"
puts viesti  # Tulostuu: Moi, Matti!

# Shovel-operattori alias <<, muuttaa alkuperäistä merkkijonoa
perusta = "Moi"
perusta << ", " << "kuinka " << "kuluu?"
puts perusta  # Tulostuu: Moi, kuinka kuluu?
```

## Deep Dive / Syväsukellus:
Merkkijonojen yhdistäminen on ollut käytössä alusta asti. Historiassa, yksinkertaiset operaattorit kuten '+' ovat olleet normi, mutta Ruby tuo tähän kaivattua joustavuutta ja tehoa.

'+': Tämä on intuitiivinen, mutta luo aina uuden merkkijonon, mikä voi olla hidas operaatio suurilla tekstimassgilla.

'#{...}': Tämä on Ruby-interpolaatio, mikä on tehokkaampi ja puhtaampi tapa yhdistää merkkijonoja kun koodista halutaan ymmärrettävämpää.

'<<': Tunnettu nimellä "shovel operator". Se muokkaa alkuperäistä merkkijonoa, ollen usein tehokkaampi, koska uutta merkkijonoa ei luoda.

On tärkeä huomata mitä metodia käytetään, sillä kaikilla on omat etunsa ja heikkoutensa eri käyttötilanteissa.

## See Also / Katso Lisäksi:
- Ruby-dokumentaatio merkkijonoista: [https://ruby-doc.org/core-3.1.0/String.html](https://ruby-doc.org/core-3.1.0/String.html)
- Ruby Style Guide merkkijonojen yhdistämisen suhteen: [https://rubystyle.guide/#string-interpolation](https://rubystyle.guide/#string-interpolation)
- Tehokas koodaus Rubyssa ja merkkijonojen käsittely: [https://www.toptal.com/ruby/ruby-metaprogramming-cooler-than-it-sounds](https://www.toptal.com/ruby/ruby-metaprogramming-cooler-than-it-sounds)
