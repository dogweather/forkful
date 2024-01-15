---
title:                "Tekstin haku ja korvaaminen"
html_title:           "Ruby: Tekstin haku ja korvaaminen"
simple_title:         "Tekstin haku ja korvaaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmoinnin tehtävät, kuten tietojen muokkaaminen, vaatii tekstiä etsimistä ja korvaamista. Rubyssa on monipuoliset työkalut tekstin etsimiseen ja korvaamiseen, mikä tekee siitä hyödyllisen ohjelmointikielen moniin tarkoituksiin.

## Miten
Alta löydät muutamia esimerkkejä Rubyssa käytettävistä toiminnoista tekstin etsimiseen ja korvaamiseen. Koodiesimerkit ja niiden tuottama lähtö näytetään "```Ruby ... ```" koodilohkoissa.

### Etsiminen tekstistä
Ruby tarjoaa useita tapoja etsiä tekstiä. Yksi yleisimmin käytetyistä on `include?`-metodi, joka tarkistaa, löytyykö annettu teksti toisesta. Esimerkiksi:

```Ruby
if "Tämä on esimerkkiteksti".include?("esimerkki")
  puts "Teksti sisältää sanan 'esimerkki'."
end
```

Tulostus:
`Teksti sisältää sanan 'esimerkki'.`

### Korvaaminen tekstissä
Rubylla on myös useita eri tapoja korvata teksti. Yksinkertaisin tapa on käyttää `gsub`-metodia, joka korvaa kaikki annetun tekstin esiintymät toisella tekstillä. Esimerkiksi:

```Ruby
teksti = "Tämä on esimerkkiteksti"
korvattu_teksti = teksti.gsub("esimerkki", "esikuva")

puts korvattu_teksti
```

Tulostus:
`Tämä on esikuva teksti`

## Syvällinen sukellus
Ruby tarjoaa myös muita vaihtoehtoja tekstin etsimiseen ja korvaamiseen. Joitakin vaihtoehtoisia metodeja ovat muun muassa `scan`, `match` ja `sub`. Voit lukea lisää näistä metodeista Ruby-dokumentaatiosta [täältä](https://ruby-doc.org/core-2.7.1/String.html).

## Katso myös
- [Ruby-dokumentaatio](https://ruby-doc.org/core-2.7.1/String.html)
- [Codecademy: String Methods in Ruby](https://www.codecademy.com/courses/learn-ruby/lessons/string-methods-i-in-ruby)
- [TutsPlus: Rubyists Can’t String Together Text Manipulation Methods in Ruby](https://code.tutsplus.com/tutorials/rubyists-cant-string-together-text-manipulation-methods-in-ruby--net-19475)