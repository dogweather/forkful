---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Ruby: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Miksi?

On monia syitä, miksi haluat muuntaa merkkijonon pienaakkosiin Rubyssä. Yksi tärkeimmistä syistä on tietojen yhdenmukaistaminen, mikä on erityisen tärkeää tietokannoissa ja tiedostojen hallinnassa. Pienaakkoset myös helpottavat merkkijonon vertailua ja manipulointia.

# Miten?

```Ruby
string = "TÄMÄ ON MERKKIJONO"
puts string.downcase
```
Tämä yksinkertainen koodinpätkä muuntaa merkkijonon "TÄMÄ ON MERKKIJONO" pienaakkosiksi ja tulostaa sen konsoliin "tämä on merkkijono". Voit myös tallentaa muunnetun merkkijonon uuteen muuttujaan, esimerkiksi `lowercase_string = string.downcase`.

# Syväsukellus

Rubyssä on monia käteviä tapoja muuntaa merkkijono pienaakkosiin. Voit käyttää esimerkiksi `downcase`-metodia, joka muuntaa kaikki merkit pienaakkosiksi, sekä `capitalize`-metodia, joka muuntaa vain merkkijonon ensimmäisen kirjaimen isoksi. Voit myös käyttää `swapcase`-metodia, joka muuntaa kaikki pienaakkoset isoiksi ja päinvastoin. Lisäksi voit käyttää myös `downcase!`, `capitalize!` ja `swapcase!` -metodeja, jotka muuntavat merkkijonon alkuperäiseen muuttujaan sen sijaan, että palauttaisivat uuden muutetun merkkijonon.

# Katso myös

- [Ruby String Documentation](https://ruby-doc.org/core-2.7.2/String.html)
- [Ruby Methods Cheat Sheet](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-methods/cheatsheet)
- [Ruby on Rails Tutorial: Learn Ruby and Rails from Scratch](https://www.railstutorial.org/book)