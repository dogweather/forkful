---
title:    "Ruby: Merkkijonon muuntaminen pieniksi kirjaimiksi"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi muuttaa merkkijonon pieniksi kirjaimiksi?

Muuttamalla merkkijonon pieniksi kirjaimiksi voit helpottaa tiedon vertailua ja käsittelyä. Pienet kirjaimet varmistavat myös, että merkkijonoja voidaan vertailla oikein, sillä Ruby-ohjelmointikieli on case-sensitive eli se huomioi kirjainten koon.

## Miten

```ruby
puts "TÄMÄ ON MERKKIJONO".downcase
puts "tämä on merkkijono".downcase
```

Seuraava koodinpätkä tulostaa molemmissa tapauksissa "tämä on merkkijono". Pilkkuja ja muita erikoismerkkejä ei muuteta, vaan ne pysyvät samanlaisina.

Tärkeää on muistaa käyttää `downcase`-metodia, joka muuttaa merkkijonon pieniksi kirjaimiksi. Ilman sitä, merkkijono pysyy ennallaan eikä muutosta tapahdu.

## Syväsukellus

Merkkijonon muuttamiseen pieniksi kirjaimiksi vaikuttaa myös ohjelmiston käyttöjärjestelmä. Esimerkiksi Windows-käyttöjärjestelmässä pienet kirjaimet saattavat muuttua eri tavalla kuin Linux-ympäristössä.

Rubyssa on myös muita tapoja muuttaa merkkijonon kirjaimia, kuten `capitalize`, joka muuttaa vain merkkijonon ensimmäisen kirjaimen isoksi ja muut pieniksi.

## Katso myös

- [Ruby String Documentation](https://ruby-doc.org/core-3.0.2/String.html)
- [Ruby Case Conversion Methods](https://www.rubyguides.com/2018/05/ruby-case-conversion-methods/)
- [Ruby Tutorial - Strings](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Ruby Style Guide - Case Conversion](https://rubystyle.guide/#case-conversion)