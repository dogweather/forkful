---
title:    "Ruby: Tiedostojen lukeminen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Miksi

Lukeminen on yksi tärkeimmistä taidoista, joita tarvitsemme elämässämme. Samoin kuin muiden tietotekniikan taitojen, lukemisen taito on myös tärkeä ohjelmoinnin maailmassa. Yksi yleinen tehtävä ohjelmoinnissa on teksti-tiedoston lukeminen ja tässä blogipostissa opimme, miten voit tehdä sen käyttäen Rubya.

## Kuinka tehdä se

Aloitetaan luomalla uusi Ruby-tiedosto ja tallentamalla se haluamaasi kansioon. Tämän jälkeen voimme aloittaa koodin kirjoittamisen.

```Ruby
# Avaa tiedosto "tekstitiedosto.txt" ja tallenna sen sisältö muuttujaan "teksti"
teksti = File.read("tekstitiedosto.txt")

# Tulosta muuttujan "teksti" sisältö
puts teksti
```

Yllä oleva esimerkki olettaa, että tekstitiedosto nimeltä "tekstitiedosto.txt" löytyy samasta kansioista kuin Ruby-tiedosto, jossa koodimme sijaitsee. Jos haluat lukea tiedoston toisesta sijainnista, voit antaa koko polun "File.read" -funktion parametriksi.

Voit myös käyttää "File.readlines" -funktiota, joka palauttaa tekstitiedoston rivit taulukkona. Tämä on hyödyllistä, jos haluat käsitellä tiedoston sisältöä rivi kerrallaan.

```Ruby
# Avaa tiedosto "tekstitiedosto.txt" ja tallenna sen sisältö taulukkoon "rivit"
rivit = File.readlines("tekstitiedosto.txt")

# Tulosta taulukon "rivit" sisältö yksi rivi kerrallaan käyttäen "each" -metodia
rivit.each do |rivi|
  puts rivi
end
```

## Syvällinen sukellus

Ruby tarjoaa myös muita tapoja lukea ja käsitellä tekstitiedostoja. Voit esimerkiksi kääriä "File" -luokan metodit "open" -funktioksi, joka automaattisesti sulkee tiedoston lukemisen jälkeen. Voit myös käyttää "each_line" -metodia, joka palauttaa rivin kerrallaan ja sallii sinun käsitellä riviä ennen kuin siirryt seuraavaan.

```Ruby
# Avaa tiedosto "tekstitiedosto.txt" ja käsittele rivi kerrallaan
File.open("tekstitiedosto.txt") do |tiedosto|
  tiedosto.each_line do |rivi|
    puts rivi.upcase # tulostaa rivin isoilla kirjaimilla
  end
end
```

Lisätietoja teksti-tiedostojen lukemisesta Rubylla voit löytää Ruby-dokumentaatiosta.

## Katso myös

- [Ruby-dokumentaatio: tiedostojen lukeminen ja kirjoittaminen](https://ruby-doc.org/core-2.6/File.html)
- [Ruby-dokumentaatio: /bin/open-uri](https://ruby-doc.org/stdlib-2.6.3/libdoc/open-uri/rdoc/OpenURI.html)