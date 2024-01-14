---
title:                "Ruby: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Miksi 

Tekstien etsiminen ja korvaaminen on tärkeä osa Ruby-ohjelmointia. Se auttaa meitä löytämään ja muokkaamaan tiettyjä sanoja tai lauseita koodissamme, mikä tekee siitä helpompaa ja tehokkaampaa. 

# Kuinka 

Ruby-kielellä voit suorittaa tekstien etsimisen ja korvaamisen yksinkertaisesti käyttämällä `gsub` -metodia. Se ottaa vastaan kaksi argumenttia: ensimmäisenä etsittävän tekstin ja toisena korvaavan tekstin. Alla on esimerkki koodista ja sen tulosteesta:

```Ruby
# Luodaan muuttuja, joka sisältää lauseen: "Tämä on Ruby-ohjelmointia."
text = "Tämä on Ruby-ohjelmointia."

# Etsitään ja korvataan "ohjelmointia" sanalla "kielen."
teksti.gsub! ("ohjelmointia", "kielen")

# Tulostetaan muokattu lause
puts teksti 

```

Tulostus: "Tämä on Ruby-kielen."

Tämä esimerkki osoittaa, kuinka helposti tekstin etsiminen ja korvaaminen onnistuu käyttämällä `gsub` -metodia. Voit myös käyttää `gsub` -metodia muuttujien ja olioiden sisällä, jotta voit vaihtaa tekstiä dynaamisesti ohjelman suorituksen aikana. 

# Syvällisempi sukellus 

On tärkeää huomata, että `gsub` -metodi muokkaa tai korvaa alkuperäisen muuttujan sisältöä. Jos haluat säilyttää alkuperäisen muuttujan ja tehdä korvauksen kopioon siitä, voit käyttää `gsub` -metodin sijaan `gsub!`. Tämä takaa, että alkuperäinen muuttuja ei muutu.

Lisäksi `gsub` -metodi voi hyödyntää myös säännöllisiä lausekkeita. Tämä antaa sinulle enemmän mahdollisuuksia monimutkaisempiin ja yksityiskohtaisempiin etsintöihin ja korvauksiin. Voit myös käyttää `gsub` -metodia yhdessä `gsub!` -metodin kanssa, jotta voit muokata alkuperäistä muuttujaa ja samalla käyttää säännöllisiä lausekkeita. Syvällisemmät säännöllisten lausekkeiden opetusohjelmat ovat kuitenkin niiden oman aiheensa, joten suosittelemme tutustumaan niihin erikseen, jos haluat oppia lisää.

# Katso myös 

- [Ruby - Dokumentaatio tekstien etsimisestä ja korvaamisesta](https://ruby-doc.org/core-2.5.0/String.html#method-i-gsub)
- [Ruby-oppitunti säännöllisiä lausekkeita varten](https://rubylearning.com/satishtalim/ruby_regular_expressions.html)
- [Ruby - Ruby on Rails Tutorial: Tekstien etsiminen ja korvaaminen](https://www.railstutorial.org/book/rails_flavored_ruby)