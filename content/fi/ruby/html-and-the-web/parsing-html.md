---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:06.150297-07:00
description: "Kuinka: J\xE4sent\xE4\xE4ksesi HTML:\xE4\xE4 Rubylla, asenna 'Nokogiri'-gem\
  \ komennolla `gem install nokogiri`. Nokogiri on kuin Sveitsin armeijan linkkuveitsi\
  \ HTML:n ja\u2026"
lastmod: '2024-03-13T22:44:57.084516-06:00'
model: gpt-4-0125-preview
summary: "J\xE4sent\xE4\xE4ksesi HTML:\xE4\xE4 Rubylla, asenna 'Nokogiri'-gem komennolla\
  \ `gem install nokogiri`."
title: "HTML:n j\xE4sennys"
weight: 43
---

## Kuinka:
Jäsentääksesi HTML:ää Rubylla, asenna 'Nokogiri'-gem komennolla `gem install nokogiri`. Nokogiri on kuin Sveitsin armeijan linkkuveitsi HTML:n ja XML:n käsittelyyn Rubyssa. Tässä nopea esimerkki:

```ruby
require 'nokogiri'
require 'open-uri'

# Lataa HTML-sisältö verkkosivulta
html_content = URI.open('http://example.com').read

# Jäsennä HTML
doc = Nokogiri::HTML(html_content)

# Poimi otsikko
title = doc.xpath('//title').text
puts "Sivun otsikko on: #{title}"
```

Tämä tuottaa jotain tällaista: `Sivun otsikko on: Esimerkkidomain`.

## Syväsukellus
Ruby-maailmassa alkuaikoina HTML:n jäsentämisen vaihtoehdot olivat rajalliset. REXML oli sisäänrakennettu mutta hidas. Sitten tuli Hpricot, mutta se hiipui pois. Nokogiri debytoi vuonna 2008, yhdistäen Hpricotin helppokäyttöisyyden libxml:n nopeuden ja tehon, todistetusti tehokkaan XML-työkalupakin.

Jäsentämismaailmassa on aina vaihtoehtoja. Jotkut vannovat sisäänrakennetun 'rexml'-kirjaston tai 'oga', toisen Rubyyn tarkoitetun XML/HTML-jäsentäjän nimeen. Mutta Nokogiri pysyy suosikkina sen kestävyyden ja nopeuden vuoksi, puhumattakaan sen laajasta ominaisuusvalikoimasta.

Kulissien takana Nokogiri muuntaa HTML:n Document Object Model (DOM) -puurakenteeksi. Tämä tekee elementtien navigoinnista ja muokkaamisesta helppoa. XPath- ja CSS-valitsimien avulla voit tarkasti löytää tarvitsemasi tiedon.

## Katso myös
- Nokogiri-gem: [https://nokogiri.org/](https://nokogiri.org/)
- Rubyn rexml-dokumentaatio: [https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html](https://ruby-doc.org/stdlib-2.6.3/libdoc/rexml/rdoc/REXML/Document.html)
- Vaihtoehtoinen jäsentäjä 'oga': [https://github.com/YorickPeterse/oga](https://github.com/YorickPeterse/oga)
- Opi lisää XPathista: [https://www.w3schools.com/xml/xpath_intro.asp](https://www.w3schools.com/xml/xpath_intro.asp)
