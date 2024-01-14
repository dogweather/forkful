---
title:                "Ruby: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lukisi komentokehoteen argumentit Ruby-ohjelmoinnin yhteydessä? Komentokehotteen argumentit ovat käytännöllisiä ja tehokkaita tapoja antaa ohjelmalle tietoa suorituksen aikana.

## Miten

Kommentokehotteessa käyttämistäsi argumenteista voit antaa ohjelmalle tietoa suorituksen aikana käyttämällä Rubyn ARGV-muuttujaa. Se sisältää taulukon kaikista annetuista argumenteista. Alla on esimerkki kodista ja tulosteesta:

```Ruby
# Koodiesimerkki
ARGV.each do |argument|
  puts "Antamasi argumentti oli: #{argument}"
end

# Komentokehotteen syöttö
ruby ohjelma.rb ensimmäinen toinen kolmas

# Tuloste
Antamasi argumentti oli: ensimmäinen
Antamasi argumentti oli: toinen
Antamasi argumentti oli: kolmas
```

## Syväsukellus

Komentokehotteen argumenttien lukeminen voi tuntua yksinkertaiselta, mutta se voi todella parantaa ohjelmasi toimivuutta ja joustavuutta. Voit myös käyttää ARGV-muuttujaa ohjelmasi sisään lataamien tiedostojen lukemiseen ja muihin monipuolisiin tarkoituksiin.

## Katso myös

- [Ruby ARGV-dokumentaatio](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Ohjeita Ruby-ohjelmoinnin aloittamiseen](https://www.codecademy.com/learn/learn-ruby) 
- [Ruby syntaksin perusteet](https://www.rubyguides.com/2018/03/ruby-syntax-cheat-sheet/)