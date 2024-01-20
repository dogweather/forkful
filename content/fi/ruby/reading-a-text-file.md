---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstitiedoston lukeminen tarkoittaa tietojen saamista tiedostosta ohjelmointikielellä. Ohjelmoijat tekevät tämän tiedon käsittelyn ja analysoinnin mahdollistamiseksi.

## Näin se tehdään:

Seuraavassa on Ruby-koodiesimerkki tekstitiedoston lukemisesta.

```Ruby
# Avaa tiedosto lukutilassa
tiedosto = File.open("esimerkki.txt", "r")

# Lue tiedoston sisältö
sisalto = tiedosto.read

# Tulosta tiedoston sisältö
puts sisalto

# Sulje tiedosto
tiedosto.close
```

Kun ajat tätä koodia, se tulostaa "esimerkki.txt"-tiedoston sisällön.

## Syvempi sukellus:

1. Historiallinen Konteksti: Ruby tarjoaa joukon tehokkaita rakenteita tiedostojen käsittelyyn. Sen syntaksi on suunniteltu olemaan yksinkertainen ja selkeä, mikä tekee tiedostojen lukemisesta suoraviivaista.
2. Vaihtoehdot: Rubyssa voit myös käyttää `File.readlines` tai `File.foreach` metodeja tiedoston rivien lukemiseen.
3. Toteutuksen Yksityiskohdat: Kun tiedosto avataan Rubyssa, se luo File-objektin. `read`-metodi palauttaa tiedoston sisällön merkkijonona. On tärkeää aina sulkea tiedosto `close`-metodilla ohjelman lopussa.

## Katso myös:

1. Ruby-Dokumentaatio: [File Class](https://ruby-doc.org/core-2.7.0/File.html)
2. Opiskelu Materiaali: [Learn Ruby - File I/O](https://www.learnrubyonline.org/en/File-I%2FO)
3. Foorumi: [Stack Overflow - Ruby](https://stackoverflow.com/questions/tagged/ruby)