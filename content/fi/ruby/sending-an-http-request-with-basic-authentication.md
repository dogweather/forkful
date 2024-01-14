---
title:                "Ruby: Lähettäminen http-pyyntö perusautentikoinnilla."
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnilla."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeää, koska se antaa käyttäjille mahdollisuuden turvallisesti kommunikoida ja kirjautua sisään verkkosivustoille ja sovelluksiin. Tämä autentikointimenetelmä auttaa varmistamaan, että vain oikeat käyttäjät pääsevät tarkoitetuille sivuille ja että arkaluontoiset tiedot pysyvät suojattuina.

## Kuinka tehdä

Perusautentikoinnin käyttö HTTP-pyynnöissä Rubyssa on helppoa. Sinun täytyy vain lisätä "Authorization" -otsakkeeseen Base64-koodattu käyttäjätunnus ja salasana. Käytetään esimerkiksi Net::HTTP-kirjastoa lähettämään GET-pyynnön GitHubin API:in:

```Ruby
require 'net/http'
require 'base64'

uri = URI('https://api.github.com/users/octocat')

user = 'käyttäjätunnus'
password = 'salasana'

request = Net::HTTP::Get.new(uri)
request['Authorization'] = "Basic #{Base64.strict_encode64("#{user}:#{password}")}"

response = Net::HTTP.start(uri.hostname, uri.port, use_ssl: uri.scheme == 'https') do |http|
  http.request(request)
end

puts response.body
```

Tuloksena oleva vastaus tulostuu konsoliin ja sisältää käyttäjän octocat tietoja GitHubista.

## Syvempi sukellus

Perusautentikoinnin toimintaperiaate on hyvin yksinkertainen. Käyttäjätunnus ja salasana koodataan Base64-muotoon ja lähetetään "Authorization" -otsakkeessa HTTP-pyynnössä. Tämä autentikointitapa on turvallisempi kuin selkeä teksti, mutta siitä huolimatta se ei ole täysin suojaamaton. On aina tärkeää varmistaa, että salasana on vahva ja käyttäjätunnus ei ole helposti arvattavissa.

## Katso myös

- [Ruby-kirjasto Net::HTTP](https://ruby-doc.org/stdlib-3.0.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Base64 Encoding](https://www.base64encode.org/)