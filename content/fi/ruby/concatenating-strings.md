---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonojen yhdistäminen on toiminto, jossa kaksi tai useampi merkkijono liitetään yhteen, muodostaen yhden yhtenäisen merkkijonon. Ohjelmoijat tekevät näin määrittääkseen tai muokatakseen sisältöä dynaamisesti koodissaan.

## Miten tehdään:

Rubyssä merkkijonot voidaan yhdistää (+) operaattorin avulla:

```Ruby
tervehdys = "Moi " + "sinulle!"
puts tervehdys
```

Tämä tuottaa tulostuksen: `Moi sinulle!`

Toinen vaihtoehto on käyttää `(<<)` operaattoria:

```Ruby
tervehdys = "Moi "
tervehdys << "sinulle!"
puts tervehdys
```

Tämä tuottaa myös tulostuksen: `Moi sinulle!`

## Syvempi tarkastelu:

Ruby syntaksi on muuttunut vuosien saatossa, mutta merkkijonojen yhdistämisen periaate on pysynyt samana. Yllämainittu `(<<)` operaattori on myös vähemmän resurssihakuinen, sillä se muokkaa alkuperäistä merkkijonoa luomatta uutta; tämä on hyödyllistä isojen tekstimassojen käsittelyssä.

Vaihtoehtoisesti voit käyttää `concat()` funktiota, joka toimii samalla tavalla kuin `(<<)` operatori:

```Ruby
tervehdys = "Moi "
tervehdys.concat("sinulle!")
puts tervehdys
```

Tämä tuottaa tulostuksen: `Moi sinulle!`

Rubyssä on myös `join()` menetelmä, jota käytetään lähinnä taulukoissa:

```Ruby
tervehdys = ["Moi", "sinulle!"].join(' ')
puts tervehdys
```

Tämä tuottaa tulostuksen: `Moi sinulle!`

## Katso myös:

- Ruby Dokumentaatio: String Concatenation: https://ruby-doc.org/core-2.7.0/String.html#method-i-3C-3C
- Ruby Style Guide: https://rubystyle.guide/
- StackOverflow: How do I concatenate strings in Ruby: https://stackoverflow.com/questions/4675263/how-do-i-concatenate-strings-in-ruby