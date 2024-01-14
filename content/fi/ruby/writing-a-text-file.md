---
title:                "Ruby: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittaminen ja lukeminen ovat kaksi tärkeintä osaa ohjelmointia. Tekstin tallentaminen tiedostoon on tapa tallentaa ja lukea tietoja ohjelmista, ja se on yksi tärkeimmistä taidoista, joita jokaisen ohjelmoijan on hallittava.

## Kuinka kirjoittaa tekstitiedosto Rubylla

Ruby-ohjelmoinnissa on useita tapoja kirjoittaa tekstitiedostoja, mutta yksinkertaisin tapa on käyttää `File`-luokkaa. Ensinnäkin, avaa tiedosto `testi.txt` kirjoittamista varten käyttämällä `File.open`-metodia. Tämän jälkeen voit kirjoittaa haluamasi tekstin tiedostoon käyttämällä `puts`-komentoa. Lopuksi sulje tiedosto `File.close`-metodilla.

```Ruby
File.open('testi.txt', 'w') do |tiedosto|
    tiedosto.puts "Tämä on tekstiä, joka tallennetaan tiedostoon."
end
```

Tämän jälkeen voit lukea tiedoston käyttämällä `File.read`-metodia ja tulostaa sen sisällön komentoriville.

```Ruby 
puts File.read('testi.txt')
```

Tämän avulla voit tallentaa ja lukea tietoja tekstitiedostoista Rubylla.

## Syvällinen sukellus

Kirjoittaessa tekstitiedostoja Rubylla, on tärkeää olla huolellinen tiedoston avaamisessa ja sulkemisessa. Jos unohdat sulkea tiedoston, se voi aiheuttaa ongelmia ja jopa kadottaa tietoja. Voit myös käyttää `File.write`-metodia kirjoittamiseen, mutta muista kuitenkin sulkea tiedosto käsin. Voit myös kokeilla eri tiedostomuotoja, kuten `.csv`-tiedostoja, ja käyttää `CSV`-kirjastoa tiedon tallentamiseksi.

## Katso myös
- [File-luokka Rubyssa](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby-tekstinkäsittely](http://ruby-doc.org/core-3.0.0/doc/syntax/calling_methods_rdoc.html)
- [Ruby CSV-kirjasto](https://ruby-doc.org/stdlib-2.5.1/libdoc/csv/rdoc/CSV.html)