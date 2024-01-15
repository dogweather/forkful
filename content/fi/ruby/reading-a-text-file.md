---
title:                "Tiedoston lukeminen"
html_title:           "Ruby: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Miksi

Text tiedostojen lukeminen on yksi perustavanlaatuisimmista taidoista, joita Ruby-ohjelmoija voi tarvita. Se mahdollistaa tiedon käsittelyn ja muokkaamisen tekstipohjaisista tiedostoista, ja on välttämätön taito monissa ohjelmointiprojekteissa.

## Miten

### Peruslukeminen

Perus tapa lukea tiedosto Rubylla on käyttää `File`-luokkaa ja sen `open`-metodia. Voit antaa `open`-metodille tiedostopolun ja valita haluamasi lukutavan. Esimerkiksi:

```Ruby
File.open("tiedosto.txt", "r") do |file|
  puts file.read
end
```

Yllä olevassa esimerkissä tiedosto `"tiedosto.txt"` avataan lukutilassa (`"r"`) ja sen sisältö tulostetaan konsoliin `puts`-metodilla.

### Vieno tapa

Yksi suosituimmista tavoista lukea tekstiä Rubylla on käyttää `File`-luokan `readlines`-metodia. Tämä palauttaa taulukon, jossa jokainen rivi on oma merkkijononsa. Voit sitten käyttää tätä taulukkoa esimerkiksi `each`-metodin kanssa tai iteroimalla sen läpi for-silmukalla:

```Ruby
File.open("tiedosto.txt", "r") do |file|
  file.readlines.each do |line|
    puts line
  end
end
```

### Vihjeitä ja nikssejä

* Muista aina sulkea tiedosto käytön jälkeen `File#close`-metodilla.
* Voit käyttää erilaisia lukutiloja tiedoston avaamiseen, kuten `"r"` (luettavaksi), `"w"` (kirjoitettavaksi) tai `"a"` (lisättäväksi).
* Jos tiedostosi käyttää ei-tavallista merkistöä (kuten UTF-8), voit asettaa `File.open`-metodin parametrikseen `"r:UTF-8"`.

## Syvä sukellus

Lue tämä osio vain, jos haluat ymmärtää paremmin, miten tiedostojen lukeminen toimii Rubylla.

Kun avaat tiedoston `File.open`-metodilla, se palauttaa `File`-luokan ilmentymän, joka luo välillistä URL:ää vastaavan luokan. Tämä luokka hoitaa tiedoston lukemisen, kirjoittamisen ja muokkaamisen.

Muutamia esimerkkejä `File`-luokan tarjoamista metodeista:

* `read`: lukee koko tiedoston yhtenä merkkijonona
* `gets`: lukee yhden rivin tiedostosta
* `each`: antaa pääsyn jokaiseen tiedoston riviin
* `write`: kirjoittaa merkkijonon tiedostoon
* `close`: sulkee tiedoston

## Katso myös

* [Ruby Doc - File-luokka](https://ruby-doc.org/core-2.6.3/File.html)
* [Ruby Monk - Tiedostojen käsittely Rubylla](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/41-file-i-o/lessons/91-reading-files)