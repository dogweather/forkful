---
date: 2024-01-20 17:44:58.324776-07:00
description: "How to: - Kuinka: Internetin alkuvuosina verkkosivujen lataus tapahtui\
  \ k\xE4sin, mutta skriptit ja ohjelmointikielet helpottivat prosessia. Rubyssa klassinen\u2026"
lastmod: '2024-04-05T22:38:57.701525-06:00'
model: gpt-4-1106-preview
summary: "- Kuinka: Internetin alkuvuosina verkkosivujen lataus tapahtui k\xE4sin,\
  \ mutta skriptit ja ohjelmointikielet helpottivat prosessia. Rubyssa klassinen tapa\
  \ on k\xE4ytt\xE4\xE4 `Net::HTTP`-kirjastoa, joka tulee Ruby standardikirjaston\
  \ mukana. Vaihtoehtona on monia helppok\xE4ytt\xF6isempi\xE4 kirjastoja, kuten `open-uri`\
  \ ja `httparty`. `Net::HTTP` kuitenkin seuraa Ruby-yhteis\xF6n minimalistista filosofiaa:\
  \ se mahdollistaa suoran kontrollin HTTP-pyynn\xF6ist\xE4 ja vastauksista selvitt\xE4\
  m\xE4tt\xF6m\xE4st\xE4 k\xE4ytt\xF6liittym\xE4st\xE4."
title: Verkkosivun lataaminen
weight: 42
---

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
