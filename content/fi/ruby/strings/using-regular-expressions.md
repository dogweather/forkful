---
title:                "Säännöllisten lausekkeiden käyttö"
aliases:
- /fi/ruby/using-regular-expressions/
date:                  2024-02-03T19:18:20.906047-07:00
model:                 gpt-4-0125-preview
simple_title:         "Säännöllisten lausekkeiden käyttö"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Säännölliset lausekkeet (regex) Rubyn kielessä ovat malleja, joita käytetään merkkijonojen merkkiyhdistelmien vastaavuuden etsimiseen, mahdollistaen kehittäjille tekstin etsimisen, vastaavuuden tarkistamisen ja manipuloinnin tehokkaasti. Ohjelmoijat käyttävät regexiä tehtäviin, kuten validointi, jäsentäminen ja merkkijonon manipulointi, tehden siitä korvaamattoman työkalun tekstin käsittelyssä.

## Kuinka:
### Perusvastaavuus
Merkkijonon vastaavuuden tarkistamiseen yksinkertaisen mallin kanssa voi käyttää `match`-metodia. Alla tarkistamme, löytyykö sana "Ruby" annetusta merkkijonosta.

```ruby
if /Ruby/.match("Hei, Ruby!")
  puts "Vastaavuus löytyi!"
end
# Tuloste: Vastaavuus löytyi!
```

### Mallivastaavuus muuttujien kanssa
Voit sisällyttää muuttujia regexiisi käyttämällä `#{}`-syntaksia, tehdessäsi malleistasi dynaamisia.

```ruby
kieli = "Ruby"
if /#{kieli}/.match("Ohjelmointi Rubylla on hauskaa.")
  puts "Puhutaan Rubysta!"
end
# Tuloste: Puhutaan Rubysta!
```

### Regexin käyttö korvaamiseen
`gsub`-metodi mahdollistaa jokaisen mallia vastaavan esiintymän korvaamisen määritellyllä korvaavalla merkkijonolla.

```ruby
puts "foobarfoo".gsub(/foo/, "bar")
# Tuloste: barbarbar
```

### Kaappaaminen
Sulkeita regexissa käytetään osien kaappaamiseen vastaavuudesta. `match`-metodi palauttaa `MatchData`-objektin, jota voit käyttää kaappausten saavuttamiseen.

```ruby
match_data = /(\w+): (\d+)/.match("Ikä: 30")
puts match_data[1] # Kaapattu otsikko
puts match_data[2] # Kaapattu arvo
# Tuloste:
# Ikä
# 30
```

### Kolmannen osapuolen kirjastojen käyttö
Vaikka Rubyn vakio kirjasto onkin tehokas, saatat joskus tarvita erikoistuneempaa toiminnallisuutta. Yksi suosittu gem regexin kanssa työskentelyyn on `Oniguruma`, joka tarjoaa lisäominaisuuksia Rubyn sisäänrakennettuun regex-moottoriin nähden.

Asenna se käyttäen:
```bash
gem install oniguruma
```

Esimerkki käytöstä voisi näyttää tältä (olettaen, että olet vaatinut `oniguruman` asentamisen jälkeen):

```ruby
# Tämä on kehittyneempi esimerkki ja saattaa vaatia lisäasetuksia
require 'oniguruma'

malli = Oniguruma::ORegexp.new('(\d+)')
match_data = malli.match("Numero on 42.")
puts match_data[1]
# Tuloste: 42
```

Muista, vaikka säännölliset lausekkeet ovat voimakkaita, ne voivat muuttua monimutkaisiksi ja vaikeasti hallittaviksi monimutkaisemmissa malleissa. Tavoittele luettavuutta ja harkitse vaihtoehtoisia menetelmiä, jos regexisi muuttuu liian monimutkaiseksi.
