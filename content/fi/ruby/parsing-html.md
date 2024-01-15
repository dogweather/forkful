---
title:                "HTML:n jäsentäminen."
html_title:           "Ruby: HTML:n jäsentäminen."
simple_title:         "HTML:n jäsentäminen."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

HTML-parsettaminen (parsing) on tärkeä osa verkkosovellusten kehitystä. Se mahdollistaa tietojen keräämisen ja käsittelyn verkkosivuilta ja palvelimilta, mikä voi helpottaa esimerkiksi tiedon analysointia ja sisällön automaattista muokkaamista.

## Kuinka

HTML-parsettaminen on helppoa Rubyssa käyttäen Nokogiri-kirjastoa. Ensiksi asenna kirjasto ja vaadittavat alikirjastot:

```
gem install nokogiri
gem install open-uri
```

Sitten voit ladata haluamasi verkkosivun URL-osoitteesta ja tallentaa sen muuttujaan käyttämällä `open-uri`-alikirjastoa:

```
require 'open-uri'
url = 'http://www.example.com'
page = open(url)
```

Seuraavaksi luo uusi Nokogiri-olio, joka käyttää verkkosivun HTML-koodia:

```
require 'nokogiri'
doc = Nokogiri::HTML(page)
```

Nyt voit navigoida HTML-sisällössä käyttämällä CSS-valitsimia ja hakea haluamiasi tietoja:

```
doc.css('h1').text # hakee ensimmäisen h1-elementin tekstin verkkosivulta
doc.css('.link').each do |link| # hakee kaikki class="link" elementit ja tulostaa niiden tekstin
  puts link.text.strip
end
```

## Syvällinen sukellus

Nokogiri-kirjasto on erittäin monipuolinen ja tarjoaa monia erilaisia työkaluja HTML-parsettamiseen. Voit esimerkiksi käyttää XPath-valitsimia, mikä mahdollistaa tarkemman tiedonhaun. Lisäksi kirjaston dokumentaatio tarjoaa hyödyllistä tietoa sen käytöstä ja tarjoaa erilaisia esimerkkejä.

## Katso myös

- [Nokogiri - Ruby-kirjasto HTML-parsettamiseen](https://github.com/sparklemotion/nokogiri)
- [OHM - Ruby DSL tietokantamallinnukseen](https://github.com/soveran/ohm)
- [Rails-paketti HTML-parsettamiseen](https://github.com/rails/rails-html-sanitizer)