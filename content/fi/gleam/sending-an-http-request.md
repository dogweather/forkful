---
title:                "HTTP-pyynnön lähettäminen"
date:                  2024-01-20T17:59:45.047949-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
HTTP-pyynnön lähettäminen tarkoittaa pyynnön tekemistä palvelimelle webissä. Ohjelmoijat tekevät tämän, jotta he voivat vaihtaa dataa palveluiden välillä.

## How to: - Kuinka:
```gleam
import gleam/http
import gleam/httpc
import gleam/should

fn send_request() {
  let response = httpc.send(http.Request(
    method: http.Get,
    url: "https://api.example.com/data",
    headers: [],
    body: None,
  ))
  
  match response {
    Ok(res) -> "Request successful! Status: " ++ int.to_string(res.status)
    Error(err) -> "Something went wrong: " ++ err.message
  }
}

fn main() {
  let result = send_request()
  should.equal(result, Ok("Request successful! Status: 200"))
}
```

## Deep Dive - Syväsukellus:
HTTP-pyynnöt ovat web-ohjelmoinnin peruskiviä. Ennen HTTP/1.0:n vakiinnuttamista 1996, data siirrettiin verkon yli monilla eri tavoilla, joista moni oli räätälöity yhteen käyttötarkoitukseen. Nykyaikaiset kielet ja työkalut, kuten Gleam, tarjoavat sisäänrakennetut kirjastot HTTP-pyyntöjen tekemiseen, mikä tekee integraation eri verkkopalveluiden kanssa suhteellisen yksinkertaiseksi.

Vaihtoehtoja `httpc`:lle Gleamissa on mm. käyttää erilaisia HTTP-asiakaskirjastoja esim. `reqwest` tai erikoistuneempia työkaluja API-yhteyksiin kuten GraphQL-asiakkaat.

Yksityiskohdat: `httpc.send` palauttaa joko `Ok(response)` tai `Error(error)`, riippuen siitä onnistuiko pyyntö. Voit määrittää pyynnön metodia, URL-osoitetta, otsikoita ja sisältöä. Paketin käyttö vaatii Gleamin uusimman version, joka tukee asynkronisuutta ja virheenkäsittelyä.

## See Also - Katso Myös:
- Gleam HTTP client documentation: https://hexdocs.pm/gleam_httpc/gleam/httpc/
- Gleam Language Website: https://gleam.run/
- HTTP specification: https://www.ietf.org/rfc/rfc2616.txt
- More about the history of HTTP: https://www.w3.org/History.html