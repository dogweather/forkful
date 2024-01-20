---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelman tai ohjelmiston kehittämisen aloittamista tyhjästä. Ohjelmoijat tekevät tämän ratkaistakseen uuden ongelman tai luodakseen jotakin uutta.

## Kuinka:

Aloitetaan luomalla uusi tiedosto ja määritellään funktio Rubyssa seuraavasti:

```Ruby
# luo uusi tiedosto 'hello.rb'
$ touch hello.rb

# avaa tiedosto ja kirjoita seuraava funktio:
def tervehdi(nimi)
  puts "Hei, #{nimi}!"
end
```

Suoritetaan funktio komentoriviltä:

```Ruby
# suorita funktio
$ ruby -e 'require "./hello"; tervehdi("Maailma")'
```

Tulostuu "Hei, Maailma!"

## Syventävä osuus:

Aivan ensimmäiset ohjelmat tehtiin suoraan konekielellä, joka on erittäin työlästä ja virhealtista. Sittemmin on kehitetty erilaisia ohjelmointikieliä, kuten Ruby, jotka mahdollistavat ohjelmointitehtävien suorittamisen helpommin ja nopeammin.

Uuden projektin aloittamiseen on muitakin vaihtoehtoja. Voit esimerkiksi käyttää Ruby on Rails -kehystä, joka automatisoi monia tehtäviä ja nopeuttaa kehitystä.

Aloitimme projektin luomalla uuden tiedoston ja määrittämällä yksinkertaisen funktion. Tämä on yksinkertaisin tapa aloittaa. Suuremmissa projekteissa tarvitset todennäköisesti useita tiedostoja ja luokkia.

## Katso myös:

1. [Ruby-ohjelmointiopas](http://ruby-doc.org/)
2. [Ruby on Rails -opas](https://guides.rubyonrails.org/)
3. [GitHub-sivusto, jossa on Ruby-esimerkkejä](https://github.com/ruby/ruby)