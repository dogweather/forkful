---
title:                "Html-analyysi"
html_title:           "Ruby: Html-analyysi"
simple_title:         "Html-analyysi"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
HTML-parseeraus on tärkeä osa ohjelmointia, joka mahdollistaa verkkosivujen sisältämien tietojen keräämisen ja käsittelyn. Käytännön esimerkkinä tästä voisi olla vaikkapa verkkosivun sisältämien tietojen tallentaminen tietokantaan tai tiettyjen tietojen hakeminen sivulta.

## Kuinka?
```Ruby
require 'nokogiri'
require 'open-uri'

# Määritellään muuttuja, johon tallennetaan haluttu verkkosivu osoitteesta
page = Nokogiri::HTML(URI.open('https://www.example.com'))

# Käydään läpi verkkosivun kaikki linkit ja tulostetaan ne näkyviin
page.css('a').each do |link|
  puts link.text.strip
end
```
Esimerkkituloste:
```
Home
About Us
Products
Contact Us
```

## Syvemmälle
HTML:n luominen ja käsittely on kehittynyt vuosien saatossa ja nykyään siihen on useita erilaisia tapoja, kuten XML, YAML tai JSON. Nämä kaikki tarjoavat erilaisia etuja ja haittoja verrattuna HTML:ään. HTML-parsereita löytyy myös useita erilaisia, kuten Nokogiri, Hpricot ja Html-Parser.

## Katso myös
Tsekkaa lisätietoa Nokogirista: https://nokogiri.org/
Katso mitä muita vaihtoehtoja on HTML-parsereissa: https://www.ruby-toolbox.com/categories/html_parsing