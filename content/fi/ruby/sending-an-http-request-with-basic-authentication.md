---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lähettämällä HTTP-pyynnön perusautentikaation kanssa, saamme kommunikoida turvallisesti tietojen kanssa verkkosivuilla tai palvelimella. Ohjelmoijat turvautuvat tähän autentikoinnilla varustettuun lähestymistapaan tietojen eheyden, luottamuksellisuuden ja saatavuuden varmistamiseksi.

## Kuinka toimii:

```Ruby
require 'net/http'
require 'uri'

uri = URI("https://your-website.com")
http = Net::HTTP.new(uri.host, uri.port)
request = Net::HTTP::Get.new(uri.request_uri)
request.basic_auth("username", "password")
response = http.request(request)

puts response.body
```

Esimerkissämme luomme uuden HTTP-pyynnön käyttäen Net:HTTP: olion ja URI: n. Lisäämme sitten perusautentikoinnin käyttäjänimen ja salasanan avulla. Lopuksi teemme pyynnön ja tulostamme vastauksen.

## Syvällisempi Tietämys

Perusautentikoinnissa yksinkertainen, mutta vankka suojausprotokolla joka on ollut läsnä HTTP:n varhaisista päivistä lähtien. Se ei ole turvallisin vaihtoehto tiedonsiirtoon, mutta tarjoaa käyttäjille helpon tavan autentikoida itsensä.

Vaihtoehtona voimme käyttää monimutkaisempia autentikointimenetelmiä kuten Digest-autentikointi tai OAuth, jotka tarjoavat parannettua turvallisuutta.

Itse toteutuksessa Ruby käyttää Net: HTTP: n ja URI: n yhdistelmää pyynnön luomiseen ja lähettämiseen. Perusautentikointitiedot lisätään pyynnön otsikkoon Base64-koodatun merkkijonon muodossa.

## Katso myös:

1. [Net::HTTP Ruby Doc](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
2. [HTTP Autentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
3. [Digest-autentikointi](https://en.wikipedia.org/wiki/Digest_access_authentication)
4. [OAuth](https://oauth.net/)