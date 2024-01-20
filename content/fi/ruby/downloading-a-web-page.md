---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Web-sivun lataaminen on prosessi, jossa verkkosivun sisältö, mukaan lukien HTML, CSS, JS ja kuvat, tallennetaan. Ohjelmoijat tekevät tämän esimerkiksi kootakseen sivustojen dataa tai luodakseen offline-versioita verkkosivuista.

## Miten:
Rubyssa voit käyttää gemiä nimeltä 'open-uri'. Tässä esimerkki sen käyttötavasta:

```Ruby
require 'open-uri'
sivusto_data = open('http://www.esimerkki.com').read
puts sivusto_data
```

Käynnistäessäsi tämän koodin, se näyttää HTML-koodin osoitteesta 'http://www.esimerkki.com'.

## Syvempi tutkiskelu
Ennen 'open-uri'-kirjastoa Rubyssa käytettiin 'net/http'-kirjastoa. Vaikkakin se on vahva ja monipuolinen, sen käyttö voi olla monimutkaista. 'open-uri' on suunniteltu olemaan yksinkertaisempi ja helpommin käytettävissä.

Vaihtoehtoisesti, voit käyttää muita http-kirjastoja, kuten 'curb' ja 'httparty', jotka tarjoavat hieman erilaisia ominaisuuksia.

Web-sivun lataaminen tapahtuu tehden HTTP GET -pyynnön haluttuun URL-osoitteeseen. Serveri vastaa tähän pyyntöön lähettämällä takaisin HTML-tiedoston, jonka ohjelma sitten tallentaa.

## Katso myös
1. Ruby 'open-uri' dokumentaatio: [https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/open-uri/rdoc/OpenURI.html)
2. Ruby 'net/http' dokumentaatio: [https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
3. 'curb' gem: [https://rubygems.org/gems/curb](https://rubygems.org/gems/curb)
4. 'httparty' gem: [https://rubygems.org/gems/httparty](https://rubygems.org/gems/httparty)