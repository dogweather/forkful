---
title:                "Ruby: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi tekstitiedostoa ohjelmointikieltä Ruby käyttäen? Tekstitiedostojen lukeminen on tärkeä taito, jota tarvitset käsitellessäsi dataa ja tietovirtoja ohjelmointityössäsi. Lukemalla tekstitiedostoja, voit lukea, käsitellä ja tallentaa erilaisia tietoja ja käyttää niitä ohjelmistojen ja sovellusten luomisessa.

## Miten

Selitämme nyt, miten voit lukea tekstitiedostoja käyttäen Ruby-ohjelmointikieltä. Ensimmäinen askel on avata tekstitiedosto käyttäen File-luokkaa. Ja voit määrittää tiedoston, josta haluat lukea, käyttäen sen polkua. Tämän jälkeen voit käyttää `read`-metodia lukeaksesi tekstitiedoston sisällön ja tallentaa sen muuttujaan. Katso alla esimerkki:

```Ruby
tiedosto = File.open("tekstitiedosto.txt")
sisältö = tiedosto.read
```

`sisältö`-muuttuja sisältää nyt tekstitiedoston sisällön ja voit käyttää sitä tarpeidesi mukaan. Esimerkiksi voit tulostaa sen terminaalille käyttäen `puts`-metodia:

```Ruby
puts sisältö
```

Tämä tulostaa tekstitiedoston sisällön terminaalille. Voit myös käyttää muita metodeja, kuten `split` ja `map`, käsitelläksesi tekstitiedoston sisältöä haluamallasi tavalla.

## Syvempi sukellus

Voit myös määrittää tekstitiedostoon tiettyjä rajoituksia, kuten riveittäistä lukemista tai kirjoittamattomien rivien ohittamista. Voit myös lukea ja käsitellä CSV- ja JSON-tiedostoja käyttäen erityisiä kirjastoja. Näiden lisäksi voit myös kirjoittaa tekstitiedostoon tietoa käyttäen `write`-metodia.

Tutkimalla erilaisia metodeja ja käyttämällä niitä tekstitiedostojen kanssa, voit saavuttaa enemmän ymmärrystä tietojen käsittelystä ohjelmointikielessä Ruby ja laajentaa taitojasi ohjelmoinnissa.

## Katso myös

- [Ruby File-luokka](https://ruby-doc.org/core-2.7.1/File.html)
- [CSV-kirjasto Rubyssa](https://ruby-doc.org/stdlib-2.7.1/libdoc/csv/rdoc/CSV.html)
- [JSON-kirjasto Rubyssa](https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html)