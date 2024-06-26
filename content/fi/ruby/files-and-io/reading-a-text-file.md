---
date: 2024-01-20 17:55:16.685821-07:00
description: "How to - Miten tehd\xE4\xE4n: Tekstitiedoston lukeminen on perustoiminto,\
  \ joka on ollut mukana ohjelmointikieliss\xE4 alusta asti. Historiallisesti se on\
  \ t\xE4rke\xE4\xE4\u2026"
lastmod: '2024-04-05T22:51:11.245371-06:00'
model: gpt-4-1106-preview
summary: "Tekstitiedoston lukeminen on perustoiminto, joka on ollut mukana ohjelmointikieliss\xE4\
  \ alusta asti."
title: Tekstitiedoston lukeminen
weight: 22
---

## How to - Miten tehdään:
```Ruby
# Luetaan tiedosto rivi kerrallaan
File.open("esimerkki.txt", "r").each do |rivi|
  puts rivi
end

# Tiivistetysti, luetaan koko tiedosto kerrallaan
sisalto = File.read("esimerkki.txt")
puts sisalto

# Käsitellään tiedoston rivejä taulukkona
File.readlines("esimerkki.txt").each_with_index do |rivi, indeksi|
  puts "#{indeksi + 1}: #{rivi}"
end
```
Sample output:
```
Hei maailma!
Ruby on mukavaa.
Tiedostojen käsittely on hyödyllistä.
```

## Deep Dive - Syväsukellus:
Tekstitiedoston lukeminen on perustoiminto, joka on ollut mukana ohjelmointikielissä alusta asti. Historiallisesti se on tärkeää tiedon säilyttämisen ja jakelun kannalta. Rubyssa `IO`-luokka ja sen alaluokka `File` mahdollistavat tiedostojen käsittelyn. Vaihtoehtoina tiedoston lukemiselle voi käyttää kirjastoja kuten `CSV` tai `JSON`-kirjastoja, jos tiedoston formaatti vaatii erityistä käsittelyä. Tietoturvaa ei saa unohtaa: varmista aina, ettei luetuista tiedostoista voi suorittaa tahattomia komentoja.

## See Also - Katso myös:
- Ruby-Doc for IO class: [https://ruby-doc.org/core/IO.html](https://ruby-doc.org/core/IO.html)
- Ruby-Doc for File class: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html)
