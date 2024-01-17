---
title:                "Uuden projektin aloittaminen"
html_title:           "Ruby: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Uuden projektin aloittaminen tarkoittaa uuden ohjelman tai sovelluksen luomista. Ohjelmoijat tekevät tätä, jotta he voivat kehittää uutta toiminnallisuutta tai parantaa olemassa olevaa ohjelmaa.

## Miten:

```ruby
# Luodaan uusi projekti nimeltä "Esimerkki"
mkdir Esimerkki

# Siirrytään Esimerkki-kansioon
cd Esimerkki

# Alustetaan uusi Ruby-projekti
bundle init

# Luodaan uusi tiedosto nimeltä "app.rb"
touch app.rb
```

Projektin alustuksen jälkeen voimme aloittaa koodaamisen uuteen tiedostoon. Tämä antaa meille mahdollisuuden luoda uutta toiminnallisuutta ja testata sitä.

```ruby
#Lisätään koodia tiedostoon "app.rb"
puts "Tervetuloa uuteen projektiin!"

# Suoritetaan tiedosto
ruby app.rb
```

Tämä tulostaa konsoliin viestin "Tervetuloa uuteen projektiin!".

## Syvenny:

Projektien luominen on yksi ohjelmoijien tärkeimmistä tehtävistä. Se mahdollistaa uuden toiminnallisuuden luomisen ja olemassa olevan koodin parantamisen. On myös olemassa muita työkaluja, kuten Ruby on Rails, jotka auttavat kehittämään projekteja nopeasti ja tehokkaasti.

Projektin luominen Rubylla tapahtuu Bundlerin avulla. Tämä työkalu auttaa hallinnoimaan projektin riippuvuuksia ja asennuksia. Tämä tekee projektin siirtämisestä eri ympäristöihin helpompaa ja estää riippuvuuksien aiheuttamia ongelmia.

## Katso myös:

- [Bundlerin virallinen dokumentaatio](https://bundler.io/)
- [Vuoruperäinen ohjelmointi Rubylla](https://fi.wikipedia.org/wiki/Vuoroper%C3%A4inen_ohjelmointi)
- [Ruby on Rails](https://rubyonrails.org/)