---
date: 2024-01-20 18:00:48.287281-07:00
description: "How to: Rubyss\xE4 HTTP-pyynt\xF6jen l\xE4hett\xE4minen on suoraviivaista.\
  \ T\xE4ss\xE4 esimerkki k\xE4ytt\xE4en `net/http`-kirjastoa."
lastmod: '2024-03-13T22:44:57.083570-06:00'
model: gpt-4-1106-preview
summary: "Rubyss\xE4 HTTP-pyynt\xF6jen l\xE4hett\xE4minen on suoraviivaista."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

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
