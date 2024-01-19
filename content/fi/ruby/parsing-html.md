---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-tiedostojen jäsentäminen (parsing) on prosessi, jossa muutetaan web-sivun HTML-koodi joukoksi merkityksellisiä elementtejä. Ohjelmoijat tekevät tämän esimerkiksi tiedon kaappaamiseksi tai sivuston rakenteen ymmärtämiseksi.

## Miten:

Rubyssa voit käyttää Nokogiri-gemistön avulla HTML:n jäsentämistä. Tässä on perusesimerkki:

```ruby
require 'nokogiri'
require 'open-uri'

# Avaa sivu ja luo Nokogiri-objekti
doc = Nokogiri::HTML(open('http://www.example.com'))

# Etsi kaikki linkit (a-tunnisteet)
links = doc.css('a')

# Tulosta jokaisen linkin teksti
links.each do |link|
  puts link.text
end
```

Tämä koodi käy läpi jokaisen linkin `www.example.com` sivulta ja tulostaa sen tekstin.

## Syväsukellus:

HTML-jäsentämisen historiallinen konteksti on syvällisesti yhteydessä webin kehittymiseen ja sen mukana tulleisiin tietorakenteisiin. Yksinkertaistettuna, HTML-jäsentämisen avulla on mahdollista ymmärtää miten web-sivut on rakennettu ja mitä tietoa ne sisältävät.

Vaihtoehtoja Ruby:n Nokogiri-kirjastolle ovat esimerkiksi `Oga` ja `Hpricot`, mutta Nokogiri on ehkä suosituin sen kattavien ominaisuuksiensa ansiosta.

HTML-jäsentämisen toteuttaminen riippuu siitä, minkä kirjaston tai teknologian valitset ja mitkä ovat vaatimuksesi. Joissakin tapauksissa riittää yksinkertainen säännöllinen lauseke (regex), mutta monimutkaisemmat sivustot vaativat täysiverisen HTML-jäsentimen kuten Nokogirin.

## Katso myös:

1. [Nokogiri-ohjekirja](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html)
2. [Oga-ohjekirja](https://github.com/YorickPeterse/oga)
3. [Hpricot](https://github.com/hpricot/hpricot/wiki)