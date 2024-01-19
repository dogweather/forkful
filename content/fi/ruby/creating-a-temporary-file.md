---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Väliaikaisten tiedostojen luonti on prosessi, jossa luodaan tiedosto, jota käytetään vain tilapäisesti. Ohjelmoijat tekevät tämän datan tallentamiseksi väliaikaisesti, esimerkiksi prosessien välisenä puskurina tai suurien tietomäärien käsittelyä varten.

## Miten:
Rubyssa voit luoda väliaikaisen tiedoston `Tempfile`-luokalla. Alla on esimerkki:
```Ruby
   
   require 'tempfile'
  
   # Tempfile-luokan metodi new luo uuden väliaikaisen tiedoston.
   temp_file = Tempfile.new('temporary')
  
   # Nyt voimme kirjoittaa tiedostoon jotain
   temp_file.puts("Hello, World!")
  
   temp_file.close
```
Kun ajat yllä olevan koodin, se luo uuden väliaikaisen tiedoston nimeltä 'temporary', kirjoittaa siihen "Hello, World!" ja sulkee tiedoston.

## Syvällisempää
Väliaikaisten tiedostojen luomiskäytäntö on peräisin ajalta, jolloin kiintolevytila oli kallista ja rajallista. Nyt ne ovat edelleen hyödyllisiä suurten datamäärien käsittelyssä ja prosessien välisessä kommunikoinnissa. Vaihtoehtojen joukossa on myös `Tempdir`-luokka, jota käytetään luomaan väliaikaisia hakemistoja. `Tempfile` luo tiedostot tehdäkseen niistä vähemmän pysyviä: periaatteessa ne ovat poissa, kun ne suljetaan, vaikka todellisuudessa ne pysyvät, kunnes ne poistetaan muistiin kiintolevyltä.

## Katso myös
- [Tempfile-kirjaston dokumentointi](https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html)
- [Ruby's IO luokka (josta Tempfile periytyy)](https://ruby-doc.org/core-3.0.1/IO.html)
- [Ruby's File luokka](https://ruby-doc.org/core-3.0.1/File.html)
- [Tempdir-kirjaston dokumentointi](https://ruby-doc.org/stdlib-2.5.1/libdoc/tmpdir/rdoc/Dir.html#method-c-mktmpdir)