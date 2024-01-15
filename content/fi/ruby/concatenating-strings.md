---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Ruby: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on tärkeä osa ohjelmoinnin prosessia, sillä se mahdollistaa muuttujien, käyttäjän syötteen ja tekstin yhdistämisen yhdeksi kokonaisuudeksi. Täten voit luoda dynaamista ja helpommin luettavaa koodia.

## Miten

```Ruby
# Yksinkertainen tapa yhdistää kaksi merkkijonoa eli stringiä
'Hello ' + 'World'
=> "Hello World"

# Voit myös yhdistää muuttujan ja tekstin
name = 'John'
puts 'Hello ' + name
=> "Hello John"

# Voit myös käyttää '<<' -operaattoria yhdistämiseen
verb = 'Learn'
verb << 'ing'
=> "Learning"

# Voit käyttää myös 'concat' -metodia
course = "Ruby"
course.concat(" Programming")
=> "Ruby Programming"
```

## Syvällisemmin

Stringien yhdistäminen voi tapahtua eri tavoin ja se vaikuttaa myös suorituskykyyn. Yleisesti ottaen kannattaa käyttää '<<' -operaattoria tai 'concat' -metodia, sillä ne ovat nopeimpia tapoja yhdistää stringejä. Lisäksi on hyvä muistaa, että liian suuren määrän stringien yhdistäminen voi aiheuttaa suorituskyvyn heikkenemistä ja kannattaa harkita esimerkiksi käyttäjän syötteen käsittelyä eri tavalla.

## Katso myös

- [Ruby Dokumentaatio: Stringit](https://ruby-doc.org/core-2.7.0/String.html)
- [Ruby Monastery: Yhdistäminen](https://rubymonk.com/learning/books/1-ruby-primer/chapters/5-strings/lessons/33-concatenation)