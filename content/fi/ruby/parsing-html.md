---
title:                "HTML:n jäsentäminen"
date:                  2024-01-20T15:33:47.196590-07:00
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
HTML:n jäsentäminen tarkoittaa HTML-dokumentin rakenteen muuttamista käsiteltävässä muodossa, esimerkiksi haettaessa tietoja web-sivuilta. Ohjelmoijat jäsentävät HTML:ää datan kaapamiseen, automatisointiin ja verkkosivujen sisällön analysointiin.

## How to: (Kuinka tehdä:)
Jäsentämiseen Rubyssa, `nokogiri`-helmi on suosittu valinta. Asenna se käyttämällä `gem install nokogiri`, ja olet valmis aloittamaan.

```Ruby
require 'nokogiri'
require 'open-uri'

# Avaa web-sivu ja luo Nokogiri-objekti
doc = Nokogiri::HTML(URI.open('https://example.com'))

# Etsi kaikki 'h1'-otsikot
doc.css('h1').each do |header|
  puts header.content
end
```

Tämä tulostaisi jokaisen `h1` -otsikon sisällön `https://example.com` sivulta.

## Deep Dive (Sukellus syvyyksiin)
HTML:n jäsentäminen Rubyssa on tärkeä taito, joka on ollut tarpeellinen jo webin alkupäivistä. `Nokogiri` on yksi suosituimmista kirjastoista ja perustuu `libxml2` kirjastoon, joka on tehokas ja nopea XML:n ja HTML:n käsittelyyn. Vaihtoehtoisia työkaluja ovat `Oga` ja `Hpricot` (vaikkakin `Hpricot` on jo vanhentunut). Nokogiri erottuu kyvyllään käsitellä sekä fragmentteja että kokonaisia dokumentteja ja tarjoaa XPath ja CSS3-selektorit tiedon haun optimointiin.

## See Also (Katso myös)
- Nokogiri dokumentaatio: [https://nokogiri.org/](https://nokogiri.org/)
