---
title:                "Ruby: Käyttäen säännöllisiä lausekkeita"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Ruby-ohjelmoinnissa?

Säännölliset lausekkeet ovat voimallinen työkalu, jota voidaan käyttää tekstikäsittelyssä Ruby-ohjelmoinnissa. Niiden avulla voit etsiä, korvata ja muokata tekstiä tiettyjen sääntöjen perusteella, mikä tekee ohjelmoinnista helpompaa ja tehokkaampaa. Jos käytät paljon tekstin käsittelyä ohjelmissasi, säännölliset lausekkeet voivat olla ratkaisevan tärkeitä.

## Miten käyttää säännöllisiä lausekkeita Ruby-ohjelmoinnissa?

Ensimmäinen askel säännöllisten lausekkeiden käytössä on tuoda Ruby-kielen regex-kirjasto. Voit tehdä tämän lisäämällä seuraavan rivin koodiisi:

```ruby
require 'regex'
```

Seuraavaksi voit luoda säännöllisen lausekkeen antamalla sille haluamasi mallin. Esimerkiksi, jos haluat etsiä kaikki sanat, jotka alkavat kirjaimella "a", voit käyttää seuraavaa koodia:

```ruby
string = "Hei kaikki, tässä on tekstiä jolla on sanoja jotka alkavat kirjaimella a!"
regex = Regexp.new("\ba\w+")
```

Koodissa luodaan ensin muuttuja nimeltä "string", jolle annetaan haluttu teksti, ja sitten säännöllinen lauseke nimeltään "regex", joka etsii kaikki sanat, jotka alkavat bktimerkillä "a" ja jatkuvat vähintään yhden kirjaimen jälkeen. Seuraavaksi voit käyttää "string"-muuttujan "scan" metodia etsiäksesi kaikki säännöllisen lausekkeen vastaavat kohdat tekstistä:

```ruby
matches = string.scan(regex)
puts matches
```

Tämä tulostaa listan kaikista sanoista, jotka täyttävät säännöllisen lausekkeen mallin.

## Syvällisempi sukellus säännöllisten lausekkeiden käyttöön Ruby-ohjelmoinnissa

Säännöllisten lausekkeiden käyttö Rubyssa ei rajoitu vain yksinkertaisiin haku- ja korvaustoimintoihin. Voit myös käyttää niitä monimutkaisempiin tehtäviin, kuten esimerkiksi tiedostojen käsittelyyn ja muotoiluun.

Säännöllisten lausekkeiden luominen ei myöskään rajoitu vain yhdelle riville. Voit yhdistää useita lausekkeita yhteen monimutkaisten mallien luomiseksi. Lisäksi Rubyssa on useita erilaisia regex-operaattoreita ja symboleja, jotka tekevät säännöllisten lausekkeiden luomisesta joustavaa ja tehokasta.

Jos haluat oppia lisää säännöllisistä lausekkeista ja niiden käytöstä Ruby-ohjelmoinnissa, suosittelemme tutustumaan alla oleviin lisäresursseihin:

## Katso myös

- [Ruby Regex -opas](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Säännöllisten lausekkeiden opas Rubyssa](https://www.rubyguides.com/2015/08/ruby-regex/)