---
title:                "Verkkosivun lataaminen"
html_title:           "Ruby: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Web-sivun lataaminen tarkoittaa yksinkertaisesti sivun tiedon tallentamista tietokoneellesi. Ohjelmoijat tekevät tätä esimerkiksi käyttääkseen sivustolta peräisin olevaa tietoa omiin sovelluksiinsa tai tarkistaakseen sivuston ajantasaisuuden.

## Kuinka:

Rubyssa web-sivun lataaminen on helppoa. Voit käyttää siihen Ruby-mekanismia nimeltä Net::HTTP. Esimerkiksi alla oleva koodi näyttää kuinka ladataan Google.fi -sivu:

```Ruby
require 'net/http'
url = 'https://www.google.fi/'
response = Net::HTTP.get(URI(url))
puts response
```

Koodi tallentaa sivun sisällön muuttujaan ja tulostaa sen terminaaliin.

## Syväsukellus:

Web-sivujen lataamisen taustalla on HTTP-protokolla, jonka avulla verkkosivujen tiedot liikkuvat internetin yli. Ruby tarjoaa myös muita vaihtoehtoja web-sivujen lataamiseen, kuten RubyGemin avulla asennettava mechanize-kirjasto. Tämä kirjasto automatisoi sivujen lataamisen ja tiedon hakemisen, mikä voi olla hyödyllistä monimutkaisempien sovellusten kehittämisessä.

## Katso myös:

- [Ruby dokumentaatio Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/index.html)
- [RubyGems mechanize-kirjasto](https://github.com/sparklemotion/mechanize)