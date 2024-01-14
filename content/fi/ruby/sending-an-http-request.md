---
title:                "Ruby: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

Monet ohjelmoijat käyttävät Rubyä päivittäin, ja yksi tärkeimmistä komponenteista on HTTP-pyyntöjen lähettäminen ulkoisiin palveluihin. Tämä on tärkeä työkalu monille sovelluksille, jotka ovat riippuvaisia tietojen hakemisesta ja jakamisesta verkossa.

## Kuinka tehdä niin

HTTP-pyyntöjen lähettäminen Rubyllä on yksinkertaista, ja tässä on esimerkki:

```Ruby
require 'net/http'

uri = URI('http://greatblog.fi')
response = Net::HTTP.get_response(uri)

puts response.body if response.is_a?(Net::HTTPSuccess)
```

Tässä esimerkissä käytämme Net::HTTP-moduulia lähettääksemme GET-pyynnön "greatblog.fi" -sivustoon. Tämän jälkeen tulostamme vastauksen rungon, jos pyyntö onnistuu.

Pyyntöjen lähettämiseen on useita muita tapoja, kuten esimerkiksi käyttäen HTTParty- tai Faraday-kirjastoja. Nämä kirjastot tarjoavat enemmän ominaisuuksia ja joustavuutta lähettämiseen ja käsittelyyn.

## Syvä sukellus

HTTP-pyyntöjen lähettäminen koostuu useista vaiheista. Ensiksi luodaan URI-objekti, joka määrittelee pyynnön osoitteen. Sitten käytämme Net::HTTP-moduulia lähettämäänksemme pyynnön ja saamme vastauksen takaisin.

Vastauksen sisältö riippuu pyynnön tyypistä, jota käytämme. GET-pyynnöllä saatamme saada takaisin HTML-koodia, kun taas POST-pyynnöllä voimme lähettää dataa ja saada takaisin vastauksen, jossa on esimerkiksi käyttäjän tiedot. On tärkeää ymmärtää pyyntöjen eri tyypit ja millaisia vastauksia ne voivat tuottaa.

## Katso myös

- [Ruby Net::HTTP documentation](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTParty gem](https://github.com/jnunemaker/httparty)
- [Faraday gem](https://github.com/lostisland/faraday)