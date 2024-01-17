---
title:                "Perusautentikointia käyttävän http-pyynnön lähettäminen"
html_title:           "Ruby: Perusautentikointia käyttävän http-pyynnön lähettäminen"
simple_title:         "Perusautentikointia käyttävän http-pyynnön lähettäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnin kanssa tarkoittaa sitä, että lähetämme pyynnön verkkopalvelimelle, joka vaatii todentamista käyttäjänimen ja salasanan avulla. Tämä on yleinen tapa lähettää turvallisia pyyntöjä ja se on tärkeää, koska se varmistaa, että vain oikeutetut käyttäjät voivat saada pääsyn palvelimen resursseihin.

## Kuinka tehdä:

Käytämme Ruby-kieltä lähettääksemme HTTP-pyynnön perusautentikoinnin kanssa. Seuraavan esimerkin avulla näemme, miten se tehdään yksinkertaisella tavalla:

```ruby
require 'net/http'
uri = URI('http://esimerkkipalvelin.com/api')
req = Net::HTTP::Get.new(uri)
req.basic_auth('käyttäjänimi', 'salasana')
res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
puts res.body # tulosteessa näkyy vastauksen sisältö
```

## Syväsukellus:

Perusautentikoinnin käyttö on yleinen tapa turvallisten HTTP-pyyntöjen lähettämiseen, mutta on myös muita vaihtoehtoja, kuten Digest-autentikointi. Perusautentikoinnissa käytetään Base64-koodea, joka salaa käyttäjänimen ja salasanan, mutta tämä ei ole vahvin mahdollinen turvallisuusmenetelmä. Tämän vuoksi sitä suositellaan käytettäväksi vain, jos muutautentikoinnin vaihtoehdot eivät ole käytettävissä.

HTTP-pyynnön lähettämiseen perusautentikoinnin kanssa, tarvitsemme URI-osoitteen, käyttäjänimen ja salasanan. Voimme myös määrittää muita vaihtoehtoja, kuten pyyntötyypin (esimerkiksi GET, POST), päänimet ja pyynnön sisällön muodosti (esim. JSON).

## Katso myös:

- [Ruby Net::HTTP Dokumentaatio](https://ruby-doc.org/stdlib-2.6.4/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP-autentikointi Wikipediassa](https://en.wikipedia.org/wiki/Basic_access_authentication)