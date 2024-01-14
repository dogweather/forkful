---
title:                "Ruby: Tiedoston kirjoittaminen"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi?

Kirjoittamisen taidon kehittäminen on tärkeä osa jokaisen Ruby-ohjelmoijan työkalupakkia. Tekstieditorin avulla voit tallentaa koodisi pysyvästi ja jakaa sen muiden kanssa.

## Kuinka?

Ruby-koodin tallentaminen tekstitiedostoon on helppoa. Aloita luomalla uusi tiedosto, joka päättyy .rb-tunnisteeseen, esimerkiksi "tiedosto.rb". Avaa tiedosto haluamallasi tekstieditorilla, kuten Notepadilla tai Sublimella. Kirjoita koodisi ja tallenna tiedosto.

```Ruby
puts "Tervetuloa Ruby-maailmaan!"
``` 
Tämän jälkeen voit suorittaa tiedoston komennolla "ruby tiedosto.rb" ja näet tulosteen "Tervetuloa Ruby-maailmaan!" terminaalissasi.

## Syvemmälle

Kirjoittamalla tekstitiedostoon voit myös hyödyntää sen ominaisuuksia, kuten tiedostojen sisältöjen lukemista ja muokkaamista sekä tietojen tallentamista ja lataamista. Voit esimerkiksi käyttää File-luokkaa tiedoston lukemiseen ja kirjoittamiseen.

```Ruby
# Avaaminen ja lukeminen
file = File.open("tiedosto.txt", "r")
p file.read

# Kirjoittaminen
file = File.open("tiedosto.txt", "w")
file.write("Tervetuloa jälleen Ruby-maailmaan!")

# Lopetetaan
file.close
```

## Katso myös

- [Ruby - virallinen kotisivu](https://www.ruby-lang.org/fi/)
- [Sublime Text - tekstieditori](https://www.sublimetext.com/)
- [Ruby-dokumentaatio](https://ruby-doc.org/core-2.7.1/)