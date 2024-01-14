---
title:                "Ruby: Väliaikaistiedoston luominen"
simple_title:         "Väliaikaistiedoston luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Sinulla on todennäköisesti tullut vastaan tilanne, jossa tarvitset luoda väliaikaisen tiedoston Ruby-ohjelmassasi. Ehkä haluat tallentaa väliaikaisia ​​tietoja, kuten väliaikaista tietokantaa tai tallentaa väliaikaisen kuvan ennen sen lähettämistä jonnekin. Tässä blogikirjoituksessa tarkastelemme, miten voit luoda väliaikaisen tiedoston Ruby-ohjelmassasi.

## Miten

Luodaksesi väliaikaisen tiedoston Ruby-ohjelmassasi, sinun täytyy ensin vaatia "tempfile" kirjasto:

```ruby
require 'tempfile'
```

Sitten voit luoda väliaikaisen tiedoston käyttämällä "Tempfile.new" -metodia ja antamalla tiedostonimet:

```ruby
temp = Tempfile.new(['testi', '.txt'])
```

Voit nyt käyttää luotua väliaikaista tiedostoa kuten mitä tahansa muuta tiedostoa. Voit esimerkiksi kirjoittaa tai lukea tiedostoon käyttämällä "temp" -muuttujaa:

```ruby
temp.write("Tässä on esimerkki tekstiä.")
temp.close

temp.open
temp.read #=> "Tässä on esimerkki tekstiä."
```

Kun olet valmis käyttämään väliaikaista tiedostoa, muista sulkea se käyttämällä "close" -metodia.

## Syvällinen sukellus

Väliaikaisten tiedostojen luominen tapahtuu usein silloin, kun tarvitsemme väliaikaista tallennustilaa, mutta haluamme myös tiedoston poistettavan käytön jälkeen. Tätä varten voit käyttää "Tempfile.create" -metodia, joka luo väliaikaisen tiedoston ja poistaa sen automaattisesti sen käyttämisen jälkeen. Voit myös määrittää, minne väliaikainen tiedosto tallennetaan käyttämällä "dir" -parametria ja määrittelemällä haluamasi kansion polun.

```ruby
temp = Tempfile.create('testi', dir: '/kansio/polku/')
```

Voit myös määrittää väliaikaisen tiedoston nimen käyttämällä "tempfile" -parametria.

```ruby
temp = Tempfile.create('testi', tempfile: 'esimerkki.txt')
```

Syvällisempää tietoa väliaikaisista tiedostoista löydät Ruby'n virallisesta dokumentaatiosta.

## Katso myös

- [Ruby Tempfile-dokumentaatio](https://ruby-doc.org/stdlib-3.0.0/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby virallinen dokumentaatio](https://www.ruby-lang.org/fi/documentation/)