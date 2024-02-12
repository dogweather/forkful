---
title:                "HTTP-pyynnön lähettäminen"
aliases:
- /fi/ruby/sending-an-http-request/
date:                  2024-01-20T18:00:48.287281-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
HTTP-pyyntö on tapa puhua verkon yli palvelimille. Ohjelmoijat lähettävät niitä datan noutamiseen, lähettämiseen ja web-sovellusten kanssa kommunikoimiseen.

## How to:
Rubyssä HTTP-pyyntöjen lähettäminen on suoraviivaista. Tässä esimerkki käyttäen `net/http`-kirjastoa:

```ruby
require 'net/http'
require 'uri'

uri = URI('https://reqres.in/api/users')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Sample output:

```
{
    "page":1,
    "per_page":6,
    "total":12,
    "total_pages":2,
    "data":[ ... ]
}
```

Jos tarvitset enemmän toimintaa, käytä `Net::HTTP.start`. Esimerkiksi POST-pyynnölle:

```ruby
require 'net/http'
require 'uri'
require 'json'

uri = URI('https://reqres.in/api/users')
header = {'Content-Type': 'application/json'}
user = {name: 'Mikki Hiiri', job: 'Seikkailija'}

http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = (uri.scheme == 'https')
request = Net::HTTP::Post.new(uri.request_uri, header)
request.body = user.to_json

response = http.request(request)

puts response.body
```

## Deep Dive
`net/http` on Ruby-kirjasto, joka on ollut osa kieltä melkein alusta alkaen. Se tarjoaa monipuolisen tapahtumakäsittelyn HTTP-pyyntöjä varten.

Vaihtoehtoja on. `HTTParty` ja `Faraday` ovat suosittuja helppokäyttöisyytensä ja monipuolisuutensa vuoksi. Rails-sovelluksissa `ActionDispatch::Integration::RequestHelpers` tarjoaa testaustarkoituksiin metodeja HTTP-pyyntöjen tekemiseen.

HTTP-pyyntöjä lähetettäessä kannattaa huomioida SSL-sertifikaatit (turvallinen yhteys) ja aikakatkaisut, etenkin tuotanto-sovelluksissa.

## See Also
* [HTTParty GitHub repository](https://github.com/jnunemaker/httparty)
* [Faraday GitHub repository](https://github.com/lostisland/faraday)
* Reqres, simuloitu API testi- ja kehitystarkoituksiin: [Reqres](https://reqres.in/)
