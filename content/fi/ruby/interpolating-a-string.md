---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Interpoloiva merkkijono on Ruby-ohjelmoinnin tekniikka, joka sallii muuttujien tai tulosten sisällyttämisen suoraan merkkijonojen sisään. Se tehdään sujuvampaan ja helpommin luettavaan koodiin.

## Näin se tehdään:

Koodissa ympäröimme halutun muuttujan tai lausekkeen `#{}` sisään merkkijonon sisällä.

```Ruby
nimi = "Markus"
tervehdys = "Hei, #{nimi}!"
puts tervehdys 
```

Edellä oleva koodi tulostaa "Hei, Markus!", sillä interpoloimme `nimi`-muuttujan suoraan `tervehdys`-merkkijonoon.

## Syventävä sukellus

Interpolointi on ollut osa Ruby-kieltä sen alkuperäisestä versiosta lähtien. Toinen tapa tehdä sama asia on käyttää `+` tai `.concat` -menetelmää, mutta merkkijonojen interpoloinnin katsotaan olevan puhtaampaa ja tehokkaampaa. 

Interpolointi käyttää sisäisesti Ruby'n `to_s`-metodia muuttaakseen muuttujat merkkijonoiksi, joten vaikka yritetäänkin interpoloida jotakin, joka ei ole merkkijono, Ruby hoitaa sen puolestasi.

## Katso myös

1. Ruby'n virallinen dokumentaatio merkkijonojen interpoloinnista:
   http://ruby-doc.org/core/String.html#method-i-percent

2. Kattava opas merkkijonojen interpoloinnista Rubyssa:
  https://www.rubyguides.com/2018/11/ruby-string-interpolation/

3. Erikoistapaukset ja syvällinen sukellus Ruby'n merkkijonojen interpolointiin:
   https://launchschool.com/books/ruby/read/strings

Hauskaa ohjelmointia!