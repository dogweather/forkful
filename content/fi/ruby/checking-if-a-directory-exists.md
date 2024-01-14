---
title:                "Ruby: Tarkistetaan tiedoston olemassaolo"
simple_title:         "Tarkistetaan tiedoston olemassaolo"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miksi tarkistaa, jos hakemistoa on olemassa

On hyvä tapa tarkistaa, onko hakemisto olemassa ennen kuin yrität suorittaa toimia sen kanssa. Tämä auttaa välttämään virheitä ja mahdollistaa sujuvamman suorituksen.

## Miten tarkistaa, jos hakemisto on olemassa

```Ruby
if File.directory?("/hakemisto") # Tarkistaa, onko hakemisto olemassa
 puts "Hakemisto on olemassa"
else
 puts "Hakemistoa ei ole olemassa"
end
```
Tämä yksinkertainen koodinpätkä käyttää Ruby-metodia nimeltä `directory?` tarkistaakseen, onko hakemisto olemassa. Metodi palauttaa totuusarvon (true/false) ja sen avulla voit suorittaa halutut toimet sen mukaan. Jos hakemisto on olemassa, koodi tulostaa `Hakemisto on olemassa`, muuten se tulostaa `Hakemistoa ei ole olemassa`.

## Syvällinen katsaus hakemiston tarkistamiseen

Hakemiston olemassaolon tarkistaminen on tärkeä osa ohjelmointia, etenkin tiedostojen ja hakemistojen käsittelyssä. Ruby-metodi `directory?` on luotettava keino tarkistaa hakemiston olemassaolo ja sillä on myös muita hyödyllisiä sovelluksia.

Voit myös käyttää `File.exist?` -metodia tarkistaaksesi yleisesti tiedostojen ja hakemistojen olemassaolon Rubyssä.

```Ruby
if File.exist?("/tiedosto.txt") # Tarkistaa, onko tiedosto olemassa
 puts "Tiedosto on olemassa"
else
 puts "Tiedostoa ei ole olemassa"
end
```
Lisäksi voit tarkistaa, onko hakemiston sisällä tiettyä tiedostoa tai hakemistoa käyttämällä `File.exist?`- ja `File.join` -metodeja yhdessä. `join`-metodi yhdistää tiedoston / hakemiston nimen ja hakemistopolun luodaksesi kokonaisen polun.

```Ruby
if File.exist?(File.join("/documents", "raportti.pdf")) # Tarkistaa, onko tiedosto "raportti.pdf" olemassa "documents"-hakemistossa
 puts "Raportti on olemassa"
else
 puts "Raporttia ei ole olemassa"
end
```

# Katso myös

- [ruby-doc.org/core-2.6/File.html](https://ruby-doc.org/core-2.6/File.html)
- [ruby-doc.org/core-2.6/FileTest.html#method-c-directory-3F](https://ruby-doc.org/core-2.6/FileTest.html#method-c-directory-3F)
- [ruby-doc.org/core-2.6/FileTest.html#method-c-exist-3F](https://ruby-doc.org/core-2.6/FileTest.html#method-c-exist-3F)