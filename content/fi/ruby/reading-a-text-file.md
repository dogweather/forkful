---
title:                "Tekstitiedoston lukeminen"
html_title:           "Ruby: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lukeminen tekstitiedosto on tapsa ohjelmoijille lukea sisältöä tiedostosta. Tämä on hyödyllistä, kun halutaan käsitellä suuria määriä tietoa tai tallentaa tietoja pysyvästi. 

## Kuinka:
Ruby tarjoaa helpon tavan lukea tekstitiedostoja. Käytä File.open -metodia ja anna sen saada haluttu tiedoston nimi parametrina. Tallenna avattu tiedosto muuttujaan ja käytä `each` -metodia lukeaksesi tiedoston sisällön rivi kerrallaan. Esimerkki:

```Ruby
tiedosto = File.open("tekstitiedosto.txt")

tiedosto.each { |rivi| puts rivi }

#tulostaa tekstitiedoston sisällön rivi kerrallaan
```
Tiedoston lukemisen voit käyttää myös muuttujaa käyttäen ja hyödyntää `gets.chomp` -metodia lukemaan käyttäjän syötteen:

``` Ruby
tiedosto = File.open("tekstitiedosto.txt")

sisalto = tiedosto.read

kayttajan_syote = gets.chomp

puts "Käyttäjän syöttämä tieto:"
puts kayttajan_syote

#tulostaa tekstitiedoston sisällön sekä käyttäjän syöttämän tiedon
```

## Syventävä sukellus:
Tekstitiedostojen lukeminen on ollut tärkeä osa ohjelmointia jo vuosikymmenien ajan. Ennen tätä teknistä ratkaisua, ohjelmoijien täytyi käsitellä tietoa manuaalisesti, mikä oli hankalaa ja aikaa vievää. Nykyään on myös muita tapoja lukea tiedostoja, kuten käyttämällä erilaisia kirjastoja ja rajapintoja. Tiedostojen lukemisen perusteiden ymmärtäminen auttaa sinua myös ymmärtämään muita tapoja käsitellä ja tallentaa tietoja.

## Katso myös:
- [Ruby File -dokumentaatio] (https://ruby-doc.org/core-2.7.2/File.html)
- [Ruby Essentials] (https://www.rubyguides.com/ruby-tutorial/file/)
- [Ruby Text File I/O -opas] (https://www.tutorialspoint.com/ruby/ruby_input_output.htm)