---
title:                "Tulostaa virhedataa"
html_title:           "Ruby: Tulostaa virhedataa"
simple_title:         "Tulostaa virhedataa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Ruby-ohjelmointikielellä debug-viestien tulostaminen on hyödyllinen tapa tarkistaa ohjelman suoritusta ja korjata mahdollisia virheitä. Lisäksi se auttaa koodin ymmärtämisessä ja kehittämisessä.

## Miten

Debug-viestien tulostaminen Rubyssa on helppoa ja nopeaa. Voit käyttää `puts` tai `p` -metodia, joka tulostaa viestin terminaaliin. Voit myös käyttää `p` -metodia, joka tulostaa viestin lisäksi myös tiedon tietotyypistä ja muodosta. Esimerkiksi:

```Ruby
p "Tervetuloa Ruby-ohjelmointimaailmaan"
# Tervetuloa Ruby-ohjelmointimaailmaan
```

Voit myös tulostaa debug-viestin muuttujien arvoilla käyttämällä merkkijonon interpolointia tai string-metodia. Esimerkiksi:

```Ruby
name = "Maija"
puts "Hei, minun nimeni on #{name}."
p name.upcase
# Hei, minun nimeni on Maija.
#"MAIJA"
```

## Syvällinen sukellus

Debug-viestien tulostaminen on hyödyllinen taito Ruby-kehittäjille. Se auttaa tunnistamaan virheitä ja ymmärtämään ohjelman toimintaa. Voit myös käyttää `binding.pry` -metodia, joka pysäyttää ohjelman suorituksen tietyssä kohtaa ja avaa interaktiivisen konsolin, jossa voit tarkastella muuttujia ja debugata koodia.

Toinen hyödyllinen tekniikka on käyttää `ENV['DEBUG']` -asetusta, joka mahdollistaa debug-viestien tulostamisen vain silloin, kun haluat. Voit esimerkiksi asettaa sen todeksi vain kehitysympäristössä ja estää debug-viestien tulostamisen kun ohjelma on tuotantokäytössä.

## Katso myös

- [Rubyissa debug-viestien tulostaminen](https://www.rubyguides.com/2019/02/ruby-print-debug-messages/)
- [Ruby on Rails debuggaaminen](https://gorails.com/episodes/how-to-debug-ruby-on-rails-applications)