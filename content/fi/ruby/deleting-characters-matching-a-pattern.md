---
title:    "Ruby: Kaavion mukaisen merkkijoukon poistaminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Se voi olla hyödyllistä, kun käsitellään suuria määriä dataa, johon halutaan tehdä muutoksia tietyllä tavalla.

## Kuinka tehdä

```Ruby
# Esimerkki koodinpätkä

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

numbers.each do |number|
  puts number if number.odd? # Tulostaa vain parittomat numerot
end
```

Esimerkkikoodinpätkässä luomme taulukon numeroista yhdestä kymmeneen ja suoritamme sen läpi käyttäen `each` metodia. Tämän jälkeen tulostamme vain ne numerot, jotka ovat parittomia. Tätä samaa konseptia voi soveltaa myös poistamiseen, kun halutaan poistaa tiettyjä merkkejä, jotka vastaavat haluttua kaavaa.

```Ruby
# Merkkijonon merkkien poisto
string = "Hei, tässä on 123 tekstiä 456!"

clean_string = string.gsub(/[0-9]/, '') # Poistaa kaikki numerot merkkijonosta

puts clean_string # Tulostaa "Hei, tässä on tekstiä !"
```

Tässä esimerkissä käytämme `gsub` metodia poistaaksemme kaikki merkit, jotka vastaavat numerokaavaa (`[0-9]`) annetusta merkkijonosta. Lopputuloksena saamme puhtaamman merkkijonon ilman numeroita.

## Syvempi sukellus

Tämäntyyppinen toiminto on hyödyllinen esimerkiksi kun käsitellään tekstitiedostoja, joissa halutaan poistaa tietyt merkit ennen muiden toimintojen suorittamista. Samalla tavalla voi myös käsitellä tietokannan tietojen muokkaamista, jos halutaan tehdä muutoksia tiettyihin merkkeihin ennen niiden tallentamista.

## Katso myös

- [String#gsub - Ruby Dokumentaatio](https://ruby-doc.org/core-2.6.5/String.html#method-i-gsub)
- [String#delete - Ruby Dokumentaatio](https://ruby-doc.org/core-2.6.5/String.html#method-i-delete)
- [Ruby Regular Expressions - Beyond the Basics](https://code.tutsplus.com/tutorials/ruby-regular-expressions-beyond-the-basics--cms-21933)