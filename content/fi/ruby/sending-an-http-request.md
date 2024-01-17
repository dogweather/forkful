---
title:                "Http-pyynnön lähettäminen"
html_title:           "Ruby: Http-pyynnön lähettäminen"
simple_title:         "Http-pyynnön lähettäminen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Kun ohjelmoijat lähettävät HTTP-pyyntöjä, he pyytävät tietoa tai toimintaa toiselta verkkosivustolta tai palvelimelta. Tämä on yleinen keino kommunikoida verkkoyhteyksien välillä ja mahdollistaa esimerkiksi tiedon haun, käyttäjän kirjautumisen tai sivuston päivityksen.

## Miten:

```Ruby
# Lähetetään GET-pyyntö ja tulostetaan vastaus
response = Net::HTTP.get(URI('https://www.example.com'))
puts response

# Lähetetään POST-pyyntö ja tulostetaan vastaus
params = {username: 'bob', password: 'secret'}
response = Net::HTTP.post_form(URI('https://www.example.com/login'), params)
puts response.body
```

Output:
```
<!doctype html>
<html>

<head>
  <title>Esimerkkisivusto</title>
</head>

<body>
  <h1>Tervetuloa!</h1>
</body>

</html>

```

## Syväsukellus:

HTTP-pyyntöjen lähettämisellä on pitkä historia ja se on yhä tärkeä osa web-kehitystä. Tämä voidaan toteuttaa myös monilla muilla kielillä, kuten JavaScriptillä tai Pythonilla. Pyyntöjen sisältö ja parametrit vaihtelevat ja niitä käytetään muun muassa REST API:en kanssa kommunikointiin.

## Katso myös:

Lisätietoa HTTP-pyyntöjen lähettämisestä:
- [Net::HTTP dokumentaatio](https://docs.ruby-lang.org/en/3.0.0/Net/HTTP.html)
- [AJAX ja HTTP-pyyntöjen lähettäminen](https://www.tutorialspoint.com/ajax/ajax_and_http_requests.htm)