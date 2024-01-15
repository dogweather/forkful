---
title:                "Lähettäminen http-pyyntö perusautentikoinnilla"
html_title:           "Ruby: Lähettäminen http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnilla"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen perusautentikoinnin avulla on tärkeää, jos haluat suojata tietokannat ja resurssit salasanoilla. Tämä auttaa myös varmistamaan, että vain valtuutetut käyttäjät pääsevät tiettyihin sivustoihin tai sovelluksiin.

## Miten

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("example.com")
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)

# Asenna käyttäjänimi ja salasana
request.basic_auth("käyttäjänimi", "salasana")

response = http.request(request)

puts response.body
```

Tämä on esimerkki siitä, miten voit lähettää HTTP GET-pyynnön perusautentikoinnilla Ruby-kielellä. Käyttämällä `basic_auth`-metodia voit asettaa käyttäjänimen ja salasanan, jotka välitetään pyynnön mukana. Tämän jälkeen voit käyttää vastauksen `body` -attribuuttia nähdäksesi pyynnön vastauksen sisällön.

## Syventävä sukellus

Perusautentikointi on yksi monista autentikointimenetelmistä, joita voit käyttää lähettämällä HTTP-pyyntöjä Ruby-kielellä. Se toimii lähettämällä käyttäjänimen ja salasanan tietokoneen tai palvelimen kanssa varmistaen, että vain oikeilla tunnistetiedoilla varustetut käyttäjät voivat käyttää resursseja.

Voit myös käyttää muita autentikointimenetelmiä, kuten Digest-autentikointia tai OAuthia, joiden avulla voit lähettää pyyntöjä ja saada vastauksia turvallisesti. On tärkeää harkita, mikä autentikointimenetelmä sopii parhaiten käyttötarkoitukseesi ennen kuin aloitat ohjelmoinnin.

## Katso myös

- [Ruby Net::HTTP -dokumentaatio](https://ruby-doc.org/stdlib/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP-vastauksen tutkiminen Ruby-kielellä](https://www.rubyguides.com/2018/07/ruby-http-request/#How_to_Inspect_HTTP_Responses)