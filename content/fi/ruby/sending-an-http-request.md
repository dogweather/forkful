---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on prosessi, jossa tietokoneesi pyytää tietoa tai toimintoja verkkosivustolta. Ohjelmoijat tekevät tämän hakeakseen tai manipuloidakseen verkkosisältöä, kuten esimerkiksi noutaakseen tiedot API:sta tai lähettääkseen tiedot palvelimeen.

## Miten se toimii:

Saadaksemme HTTP-pyynnön toimimaan Rubyssa, tarvitsemme `net/http` -kirjaston. Tässä on yksinkertainen esimerkki HTTP GET -pyynnöstä.

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://www.example.com/search")
response = Net::HTTP.get_response(uri)

puts response.body
```

Tässä esimerkissä luomme URI-olion ja käytämme sitä `Net::HTTP.get_response` -metodin argumenttina. Tämän jälkeen tulostetaan pyynnön vastauksen sisältö.

## Tarkempi tutkinta:

HTTP-pyynnön lähettäminen on olennainen osa web-ohjelmointia ja se kehitettiin osana HTTP-protokollaa 1990-luvun alussa. Rubyssa sisäänrakennettu `net/http` -kirjasto on yleisin tapa lähettää pyyntöjä, mutta on olemassa muitakin vaihtoehtoja, kuten `httparty` tai `rest-client`.

`Net/http` toimii tekemällä TCP-yhteyden ja lähettämällä raakapyynnön palvelimelle. Voit hallita myös muita HTTP-ominaisuuksia, kuten yhteyden aikakatkaisua tai ohjata pyyntösi proxy-palvelimen kautta.

## Lue lisää:

Jos haluat syventää osaamistasi HTTP-pyynnöissä Rubyn kanssa, seuraavat linkit voivat olla hyödyllisiä:

- Ruby Documentation: Net::HTTP (https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- HTTParty Gem (https://github.com/jnunemaker/httparty)
- RestClient Gem (https://github.com/rest-client/rest-client)