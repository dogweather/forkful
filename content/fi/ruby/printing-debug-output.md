---
title:                "Ruby: Virheenkorjaustulostuksen tulostaminen"
programming_language: "Ruby"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Ruby on suosittu ohjelmointikieli monien sen ominaisuuksien, kuten dynaamisen luonteen ja ohjelmakoodin luettavuuden, ansiosta. Yksi hyödyllinen työkalu, joka auttaa kehittäjiä ymmärtämään ohjelman suoritusta ja havaitsemaan mahdollisia virheitä, on debug-tulostuksen käyttö. Tässä blogikirjoituksessa käsitellään, miksi ja miten Ruby-ohjelmoijien tulisi käyttää debug-tulostusta.

## Kuinka tehdä

Debug-tulostuksen tekeminen Rubyssa on suhteellisen yksinkertaista. Alla olevassa koodiesimerkissä käytetään `puts`-metodia tulostamaan tietoa ohjelman suorituksen aikana.

```Ruby 
def laske_summa(a, b)
  puts "Lasketaan summa #{a} ja #{b}"
  summa = a + b
  puts "Tulos on #{summa}"
end

laske_summa(3, 5)
# Tulostus:
# Lasketaan summa 3 ja 5
# Tulos on 8
```

Tässä esimerkissä ohjelmoija käyttää debug-tulostusta kahdessa vaiheessa: ensin tulostetaan laskettavat arvot ja sitten tuloksen mukaisesti. Tämä auttaa kehittäjiä varmistamaan, että ohjelman laskutoimitukset toimivat odotetusti.

## Syvempi sukellus

Debug-tulostusta voidaan käyttää myös haettaessa tietoa monimutkaisemmista ohjelman osista, kuten tietorakenteista tai muuttujista. Alla olevassa esimerkissä käytetään `p`-metodia tulostamaan muuttujan arvot.

```Ruby
kolikot = { "euro" => 1, "sentti" => 100 }
p kolikot
# Tulostus: {"euro"=>1, "sentti"=>100}
```

Tulee kuitenkin muistaa, että debug-tulostukseen käytetty koodi voi hidastaa ohjelman suoritusta. Siksi on tärkeää poistaa debug-tulostukset ennen tuotantoon siirtämistä tai käyttää niitä vain tarvittaessa.

## Katso myös

- [Ruby-debug -kirjasto](https://github.com/ruby-debug/ruby-debug)
- [Ruby-debug-tulostuksen opas](https://edgeguides.rubyonrails.org/debugging_rails_applications.html#debug-tulostuksen-käyttö)
- [Ruby-debugging perusteet](https://www.rubyguides.com/2015/03/ruby-debugging/)
- [Ruby-opas](https://ruby-lang.org/fi/documentation/)