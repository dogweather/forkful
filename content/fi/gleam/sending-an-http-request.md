---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

Title: Gleam-ohjelmoinnilla HTTP-pyyntöjen lähetys

## Mikä ja Miksi?
HTTP-pyyntö on tapa, jolla tietokoneet kommunikoivat Internetissä. Ohjelmoijat lähettävät näitä pyyntöjä, koska se on keino hakea, lähettää, päivittää ja poistaa tietoa verkkopalvelimilta.

## Kuinka tehdä:
Voit lähettää HTTP-pyynnön Gleam-koodissa käyttämällä `HttpClient`-kirjastoa näin:

```Gleam
let request = http.default.get("https://www.example.com")
let response = http.send(request)
```

Esimerkiksi, jos lähetät GET-pyynnön, saat vastauksen muodossa:

```Gleam
{body: "Hello, world!", status: 200}
```

## Syvempi tarkastelu:
HTTP-pyynnöt syntyivät 1990-luvulla, kun internet alkoi kasvaa. On olemassa monia vaihtoehtoja HTTP-pyynnoille, kuten Websockets tai GraphQL.

Implementointi Gleamilla ohjaa BEAM-virtuaalikonetta (joka käyttää Erlangia), mikä tarkoittaa, että se hyödyntää sen massiivista rinnakkaisuutta ja virheensietokykyä.

## Katso myös:
Perehdy seuraaviin lähteisiin saadaksesi tietoa aiheesta:
- Gleamin HTTP-asiakaskirjaston [ohjeet](https://gleam.run/book/tour/http-clients.html)
- Gleam [HTTP](https://hexdocs.pm/gleam_http/readme.html) moduuli Hexdocsissa
- [HTTP-pyynnöt](https://developer.mozilla.org/fi/docs/Web/HTTP/Messages) Mozilla Developer Network (MDN) sivustolla.