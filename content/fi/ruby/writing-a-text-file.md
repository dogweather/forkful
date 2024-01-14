---
title:    "Ruby: Tiedostotekstin kirjoittaminen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi 
Tekstitiedoston kirjoittaminen on tärkeä osa Ruby-ohjelmointia, sillä se mahdollistaa tiedon tallentamisen ja käsittelyn. Kirjoittamalla tekstitiedostoja voit säilyttää tietoja pysyvästi ja tehdä tiedonkäsittelyn helpoksi ja tehokkaaksi. 

## Miten 
Aloittaaksesi tekstitiedoston kirjoittamisen, sinun tulee ensin luoda uusi tiedosto. Tämän voit tehdä komennolla ```File.new("tiedostonimi.txt", "w")```, jossa annetaan tiedostolle nimi ja määritellään, että se avataan kirjoitusmoodissa. Tämän jälkeen voit kirjoittaa haluamasi tekstin tiedostoon käyttämällä ```puts```-komennon avulla. Lopuksi muista sulkea tiedosto komennolla ```close```.

Esimerkiksi, jos haluat kirjoittaa tekstiä tiedostoon nimeltä "tervetuloa.txt", koodisi voisi näyttää tältä:

```Ruby
tiedosto = File.new("tervetuloa.txt", "w")
puts "Tervetuloa Ruby-maailmaan!" 
tiedosto.close
``` 

Tämän jälkeen voit avata tiedoston ja nähdä, että teksti on tallentunut siihen onnistuneesti. Voit myös käyttää muita komentoja, kuten ```print``` ja ```write```, tekstien lisäämiseksi tiedostoon.

## Syventävä sukellus 
Vaikka tekstitiedoston kirjoittaminen saattaa aluksi tuntua yksinkertaiselta, on hyvä ymmärtää muutamia tärkeitä asioita. Ensinnäkin, kun kirjoitat uuden tiedoston, sen sisältö korvataan kokonaan, jos et käytä lisäyskomentoja, kuten ```puts```. Voit myös määritellä tiedostolle erilaisia avausmoodit, kuten "r+" (lukeminen ja kirjoittaminen) ja "a" (lisääminen loppuun).

On myös tärkeää muistaa, että tiedostot tulisi sulkea aina kun olet valmis käyttämään niitä. Tämä varmistaa, että tiedostotietokone ei jää varatuksi ja sinulla on lopuksi puhdas ja toimiva koodi.

## Katso myös 
- [Ruby Doc tiedostojen kirjoittamisesta](https://ruby-doc.org/core-2.7.0/File.html)
- [Codecademy-opetusohjelma tiedostojen käsittelystä Rubylla](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-files/) 
- [Ruby on Rails -tutoriaali tekstitiedoston kirjoittamisesta](https://www.tutorialspoint.com/ruby-on-rails/rails-file-operations.htm)