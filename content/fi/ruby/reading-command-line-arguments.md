---
title:                "Komentoriviparametrien lukeminen"
html_title:           "Ruby: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi?

Jos olet Ruby-ohjelmoija, saatat joutua käsittelemään komentoriviparametreja. Tämä artikkeli auttaa sinua ymmärtämään, miten voit lukea ja käyttää niitä ohjelmissasi.

## Miten tehdä se?

Komentoriviparametrit voidaan lukea Ruby-ohjelmassa "ARGV" muuttujan avulla. Tämä muuttuja sisältää taulukon kaikista annetuista parametreista.

```Ruby
# Luetaan parametrit 
puts ARGV

# Ajaesessa: ruby testi.rb hello world
# Tulostaa: ["hello", "world"]
```

Voit myös käyttää "each" metodia käsitelläksesi jokaisen parametrin erikseen:

```Ruby
# Tulostetaan jokainen parametri omalle riville
ARGV.each do |parametri| 
  puts parametri
end
```

## Syvällistä tietoa

Voit lisätä joustavuutta säätämällä komentoriviparametreja, kuten "required" parametreja tai argumentteja, joilla on oletusarvoja. Voit myös käyttää erilaisia kirjastoja, kuten "optparse" tai "thor", auttaaksesi käsittelyssä ja validoinnissa.

## Katso myös

- [Ruby ARGV dokumentaatio](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Optparse kirjasto](https://rubygems.org/gems/optparse)
- [Thor kirjasto](https://rubygems.org/gems/thor)