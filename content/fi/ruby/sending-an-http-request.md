---
title:                "Lähettämällä http-pyyntö"
html_title:           "Ruby: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi

Miksi haluat lähettää HTTP-pyynnön Ruby-ohjelmasta? Yksinkertaisesti sanottuna se antaa sinulle mahdollisuuden kommunikoida verkossa olevien palvelimien kanssa. Tämä avaa oven moniin jännittäviin mahdollisuuksiin, kuten web-scrapingiin, API-kutsuihin ja paljon muuhun.

# Miten

Lähetetään HTTP-pyyntö Ruby-ohjelmasta käyttämällä Net::HTTP-kirjastoa, joka on yksi Ruby-kielen ydinmoduuleista. Se lähettää pyynnön ja palauttaa vastauksen, johon voit reagoida ohjelmassasi. Katso alla oleva esimerkki:

```Ruby
require 'net/http'

# Luodaan uusi HTTP-pyyntö osoitteeseen "www.example.com"
request = Net::HTTP.get_response(URI('http://www.example.com'))

# Tulostetaan vastauksen koodi ja sisältö
puts "Response code: #{request.code}"
puts "Response body: #{request.body}"
```

Tässä esimerkissä lähetämme GET-pyynnön osoitteeseen "www.example.com" ja tulostamme vastauksen koodin (esim. 200 OK) ja sisällön (HTML-sivun).

On myös mahdollista määrittää muita HTTP-pyynnön parametreja, kuten otsikot tai pyynnön kohde. Lisätietoja löytyy Net::HTTP-dokumentaatiosta.

# Syvempi sukellus

Net::HTTP on korkean tason abstraktio HTTP-protokollasta ja sopii hyvin yksinkertaisiin pyyntöihin. Jos haluat enemmän hallintaa, voit käyttää alhaisemman tason kirjastoa, kuten Net::HTTP::Persistent tai HTTParty, jotka tarjoavat monia lisäominaisuuksia, kuten automaattisen uudelleenyrityksen tai HTTP-vastauksen hallinnan.

Lisäksi voit suorittaa muita HTTP-metodeja, kuten POST tai PUT, määrittämällä pyynnön muodon ja sisällön. Pyynnön muodon määrittäminen on erityisen tärkeää, jos haluat lähettää JSON- tai XML-tietoja palvelimelle.

# Katso myös

- [Ruby Net::HTTP dokumentaatio](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Net::HTTP::Persistent dokumentaatio](https://github.com/drbrain/net-http-persistent)
- [HTTParty dokumentaatio](https://github.com/jnunemaker/httparty)
- [Ruby- kielen virallinen nettisivu](https://www.ruby-lang.org/fi/)