---
date: 2024-01-20 18:04:28.565315-07:00
description: "Mit\xE4 ja miksi? Uuden projektin aloittaminen on tyhj\xE4lt\xE4 p\xF6\
  yd\xE4lt\xE4 alkavan sovelluskehyksen rakentamista. Koodarit aloittavat uusia projekteja\
  \ testatakseen\u2026"
lastmod: '2024-03-13T22:44:57.087257-06:00'
model: gpt-4-1106-preview
summary: "Mit\xE4 ja miksi? Uuden projektin aloittaminen on tyhj\xE4lt\xE4 p\xF6yd\xE4\
  lt\xE4 alkavan sovelluskehyksen rakentamista. Koodarit aloittavat uusia projekteja\
  \ testatakseen\u2026"
title: Uuden projektin aloittaminen
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Uuden projektin aloittaminen on tyhjältä pöydältä alkavan sovelluskehyksen rakentamista. Koodarit aloittavat uusia projekteja testatakseen ideoita, ratkaistakseen ongelmia tai luodakseen jotain uniikkia.

## How to:
Kuinka tehdään:

```Ruby
# Asenna Ruby, jos sitä ei vielä ole
# Asennusohjeet: https://www.ruby-lang.org/fi/documentation/installation/

# Luo projekti-kansio
Dir.mkdir("uusi_projekti")

# Siirry luotuun kansioon
Dir.chdir("uusi_projekti")

# Alusta Git-repositorio (vaatii Gitin asennuksen)
`git init`

# Luo tarvittavat tiedostot ja hakemistot. Esimerkiksi:
File.write('Gemfile', <<~GEMFILE)
  source 'https://rubygems.org'
  gem 'rspec' # Testikehys
end
GEMFILE

# Asenna bundler, jos sitä ei vielä ole, ja hae projektin riippuvuudet
`gem install bundler`
`bundle install`

# Alusta RSpec (testikehys)
`rspec --init`

# Kirjoita ensimmäinen testi `spec/hello_spec.rb`
File.write('spec/hello_spec.rb', <<~SPEC)
  RSpec.describe 'Hello world' do
    it 'greets the world' do
      expect('Hello, world!').to eq('Hello, world!')
    end
  end
SPEC

# Suorita testit
puts `rspec`
# => .
# => Finished in 0.00246 seconds (files took 0.11743 seconds to load)
# => 1 example, 0 failures
```

## Deep Dive:
Sukellus syvyyksiin:

Projektit alkoivat joskus paperilla ja lehtiöillä, mutta Ruby-ohjelmointia varten tarvitsemme vain tietokoneen ja idean. Historiassa kirjoitettiin paljon manuaalisesti; nykyään käytämme rakennustyökaluja, kuten Rake ja Bundler, tehokkaaseen työskentelyyn. Vaihtoehtoisesti voit käyttää Ruby on Rails -kehystä web-sovelluksille tai Sinatraa pienemmille projekteille. Käynnistämällä projekti oikeilla työkaluilla – kuten versiohallinta ja riippuvuuksien hallintajärjestelmät – saat hyvän alkuun turvalliselle ja järjestelmälliselle ohjelmistokehitykselle.

## See Also:
Katso myös:

- Ruby-dokumentaatio: https://www.ruby-lang.org/fi/documentation/
- Gitin aloitusopas: https://git-scm.com/book/fi/v2/Aloittaminen-Gitin-perusteet
- Bundler: https://bundler.io/
- RSpec: https://rspec.info/
- Rails-ohjeet: https://guides.rubyonrails.org/
- Sinatra: http://sinatrarb.com/
