---
title:                "Tilapäistiedoston luominen"
html_title:           "Ruby: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Miksi

Temporary-tiedostojen luominen on tärkeä osa useimpien ohjelmien toimintaa. Niitä käytetään esimerkiksi väliaikaisten tietojen tallentamiseen ja käsittelyyn, jolloin pääohjelma ei muutu liian raskaaksi.

## Miten

```Ruby
file = Tempfile.new("temporary.txt")

file.write("Tämä on väliaikainen tiedosto.")
puts file.path
# => /tmp/temporary20191002-12345-26x2i4.txt

file.close
```

Temporary-tiedosto luodaan Tempfile-luokan avulla ja siihen voi kirjoittaa haluamiaan tietoja. Kun tiedosto suljetaan, se tuhotaan automaattisesti ja vapauttaa sitä käyttävän muistin. Tämä tekee temporary-tiedostojen käytöstä turvallista eikä niiden poistamista tarvitse erikseen huolehtia.

## Syvällinen sukellus

Temporary-tiedoston luominen tapahtuu ohjelmassa usein käyttämällä operatiivisen järjestelmän omia työkaluja, esimerkiksi C:n "mktemp"-toimintoa. Rubyssa tätä toimintoa ei ole, vaan sen sijaan on Tempfile-luokka, joka huolehtii tiedoston luomisesta ja sen poistamisesta. Voit myös käyttää Tempfile-luokan vaihtoehtoisia luomistoimintoja, kuten `Tempfile.create`.

## Katso myös

- [Tempfile-luokan dokumentaatio](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Temporary-tiedoston käyttö Blueprint-suunnittelumallissa](https://www.oreilly.com/library/view/blueprints-for-high/9781449308024/ch04.html#tmp)
- [Tilapäistiedostojen hallinta Rubylla](https://blog.dnsimple.com/2011/10/temporary-files-and-directories-with-ruby/)