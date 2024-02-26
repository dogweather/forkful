---
date: 2024-01-20 18:02:33.782761-07:00
description: "HTTP-pyynt\xF6j\xE4 perusautentikaatiolla k\xE4ytet\xE4\xE4n, kun halutaan\
  \ p\xE4\xE4st\xE4 k\xE4siksi suojattuun resurssiin verkossa. Koodaajat k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 menetelm\xE4\xE4, jotta\u2026"
lastmod: '2024-02-25T18:49:53.990556-07:00'
model: gpt-4-1106-preview
summary: "HTTP-pyynt\xF6j\xE4 perusautentikaatiolla k\xE4ytet\xE4\xE4n, kun halutaan\
  \ p\xE4\xE4st\xE4 k\xE4siksi suojattuun resurssiin verkossa. Koodaajat k\xE4ytt\xE4\
  v\xE4t t\xE4t\xE4 menetelm\xE4\xE4, jotta\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
HTTP-pyyntöjä perusautentikaatiolla käytetään, kun halutaan päästä käsiksi suojattuun resurssiin verkossa. Koodaajat käyttävät tätä menetelmää, jotta voivat varmistaa, että käyttäjällä on oikeus nähdä tai muuttaa tietoa.

## How to: - Kuinka tehdä:
```ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/secrets')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'password'

res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}

puts res.body
```

Kun ajat tämän koodin, saat vastaukseksi HTTP-palvelimen vastauksen, joka sisältää pyydetyt tiedot tai virheilmoituksen.

## Deep Dive - Syväsukellus:
Perusautentikaatio on HTTP-protokollan vanhin autentikaatiomuoto, joka esiteltiin jo HTTP/1.0:ssa. Vaikka turvallisempiakin tapoja on olemassa, kuten OAuth ja API-avaimet, perusautentikaatio on edelleen suosittu sen yksinkertaisuuden takia. Se muodostuu käyttäjänimestä ja salasanasta, jotka lähetetään Base64-koodattuna otsikoissa.

Rubyssa HTTP-pyyntöjä ohjataan usein `Net::HTTP`-kirjaston avulla, joka on osa Ruby Standard Libraryä. Vaihtoehtoisesti voi käyttää kolmannen osapuolen kirjastoja, kuten `HTTParty` tai `RestClient`, jotka voivat tarjota lisäominaisuuksia ja syntaksia helpottamaan HTTP-pyyntöjen tekemistä.

## See Also - Katso Myös:
1. Ruby Standard Library: Net::HTTP dokumentaatio: [https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.1.0/libdoc/net/http/rdoc/Net/HTTP.html)
2. RestClient gem: [https://github.com/rest-client/rest-client](https://github.com/rest-client/rest-client)
3. HTTParty gem: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
4. RFC 7617, 'The 'Basic' HTTP Authentication Scheme': [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
