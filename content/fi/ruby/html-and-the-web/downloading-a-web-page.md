---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/ruby/downloading-a-web-page/
date:                  2024-01-20T17:44:58.324776-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Ladataan verkkosivu tarkoittaa sivun sisällön noutamista internetistä ohjelmoimalla. Ohjelmoijat tekevät tämän esimerkiksi tiedonkeruuta, sisällön analysointia tai automatisoitua testausta varten.

## How to: - Kuinka:
```Ruby
require 'net/http'
require 'uri'

def download_web_page(url)
  uri = URI(url)
  response = Net::HTTP.get_response(uri)
  return response.body if response.is_a?(Net::HTTPSuccess)
  raise "Web page couldn't be retrieved: #{response.code}"
end

# Esimerkin käyttö:
begin
  content = download_web_page('http://example.com')
  puts content[0..200] # tulostetaan vain sivun alku
rescue StandardError => e
  puts e.message
end
```

## Deep Dive - Syväkatsaus:
Internetin alkuvuosina verkkosivujen lataus tapahtui käsin, mutta skriptit ja ohjelmointikielet helpottivat prosessia. Rubyssa klassinen tapa on käyttää `Net::HTTP`-kirjastoa, joka tulee Ruby standardikirjaston mukana. Vaihtoehtona on monia helppokäyttöisempiä kirjastoja, kuten `open-uri` ja `httparty`. `Net::HTTP` kuitenkin seuraa Ruby-yhteisön minimalistista filosofiaa: se mahdollistaa suoran kontrollin HTTP-pyynnöistä ja vastauksista selvittämättömästä käyttöliittymästä.

## See Also - Katso Myös:
- HTTParty gem for simpler HTTP requests: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
