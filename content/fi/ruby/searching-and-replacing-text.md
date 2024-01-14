---
title:    "Ruby: Tekstin etsiminen ja korvaaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi vaihtaa ja korvata tekstiä Ruby-ohjelmoinnilla? Yksinkertainen vastaus on tehokkuus ja tarkkuus. Kun työskentelet suurten tekstipohjaisten tiedostojen kanssa, manuaalinen tekstinhakeminen ja korvaaminen voi olla aikaa vievää ja altistaa virheille. Ruby-ohjelmoinnilla voit automatisoida tämän prosessin ja varmistaa, että kaikki vaihdetut tekstit ovat tarkkoja ja haluamasi.

## Kuinka tehdä

Rubyilla on useita erilaisia ​​tapoja korvata tekstiä, mutta yleisimmin käytetty tapa on käyttää `gsub`-metodia. Tämä metodi korvaa kaikki parametrina annetut merkkijonot toisella merkkijonolla. Katso esimerkki alla:

```Ruby
string = "Tervetuloa Ruby-ohjelmointiin!"
string.gsub!("Ruby", "Python")
puts string
```
Tämä koodi korvaisi kaikki esiintymät merkkijonolla "Ruby" merkkijonolla "Python" ja tulostaisi "Tervetuloa Python-ohjelmointiin!".

Voit myös lisätä muuttujia `gsub`-metodiin, jotta koodi olisi joustavampi ja voit korvata erilaisia ​​tekstin osia. Esimerkiksi:

```Ruby
name = "Mika"
string = "Hei, olen #name#, kiva tavata!"
string.gsub!("#name#", name)
puts string
```
Tämä koodi tulostaisi "Hei, olen Mika, kiva tavata!".

## Syvempi tarkastelu

`gsub`-metodin lisäksi Rubyilla on myös muita tapoja vaihtaa tekstiä, kuten `sub`, `tr` ja `replace`-metodit. Näiden metodien käyttö riippuu tarkalleen mitä haluat saavuttaa ja millaisia ​​tekstiä haluat korvata.

Kun käytät säännöllisiä lausekkeita `gsub`-metodin kanssa, voit myös käyttää tarkistustaulukoita tai korvauslohkoja. Tämä auttaa sinua suorittamaan korvauksia perustuen määrätyn säännön mukaisesti, jolloin prosessi on vieläkin tarkempi ja tehokkaampi.

## Katso myös

- [Ruby-ohjelmointikielen perusteet](https://fi.wikipedia.org/wiki/Ruby)
- [Ruby Regex Cheat Sheet](https://www.rubyguides.com/2015/06/ruby-regex-cheat-sheet/)