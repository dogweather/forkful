---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Ruby: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Kun kirjoitat tekstitiedoston, tallennat tekstiä tietokoneellesi pysyvästi, jolloin voit käyttää sitä myöhemmin. Ohjelmoijat käyttävät tekstitiedostoja esimerkiksi tallentaakseen tietoa käyttäjistä tai asetuksista, tai luodakseen tietokantoja.

# Miten:

Esimerkiksi Rubyllä voit käyttää `File.open`-funktiota kirjoittaaksesi tekstitiedoston. Alla on yksinkertainen esimerkki:

```Ruby
File.open("tiedostonimi.txt", "w") { |file| file.write("Tervetuloa!") }
```

Tämä avaa tai luo tekstitiedoston nimeltä "tiedostonimi.txt" ja kirjoittaa siihen tekstin "Tervetuloa!". Voit myös käyttää manyu-perusnotaatiota `<<` lisätäksesi tietoa tekstitiedostoon:

```Ruby
File.open("tiedostonimi.txt", "a") { |file| file << "Lisää tekstiä!" }
```

Tämä lisää tiedostoon "tiedostonimi.txt" tekstin "Lisää tekstiä!" ilman että nykyistä sisältöä poistetaan. Kun olet lopettanut tiedoston käytön, muista aina sulkea se `"close"`-funktion avulla, jotta kaikki muutokset tallentuvat oikein.

# Syvä sukellus:

Tekstitiedostojen kirjoittaminen on ollut käytössä jo pitkään ja se on edelleen tärkeä osa ohjelmointia. Muut vaihtoehdot ovat esimerkiksi tietokannat, mutta tekstitiedostot ovat yksinkertainen tapa tallentaa ja jakaa tietoa.

Implementointidetaljien osalta, Rubyllä on monia muita tapoja kirjoittaa ja muokata tekstitiedostoja. Tarkista Ruby-dokumentaatio lisäohjeita varten.

# Katso myös:

- [Ruby-dokumentaatio](https://ruby-doc.org/core-2.6/File.html)
- [Tekstitiedostojen käyttäminen Rubyssa](https://www.rubyguides.com/ruby-tutorial/reading-writing-files/)
- [Miten luoda tekstitiedostoja Rubyssa](https://stackoverflow.com/questions/3689133/quickly-create-a-large-file-on-a-windows-system)