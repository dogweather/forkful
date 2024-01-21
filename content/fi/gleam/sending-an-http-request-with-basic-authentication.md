---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:01:48.532570-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Lähettääksesi HTTP-pyynnön perusautentikaation kera, lisätään käyttäjätunnus ja salasana pyynnön ylätunnisteeseen. Tämä antaa ohjelmillesi pääsyn suojattuun dataan.

## How to: - Näin teet:
```gleam
import gleam/http
import gleam/http/elli
import gleam/base64

fn basic_auth_header(user: String, password: String) -> http.Header {
  let credentials = base64.encode(user ++ ":" ++ password)
  http.header("Authorization", "Basic " ++ credentials)
}

pub fn main() {
  let auth_header = basic_auth_header("kayttaja", "salainen")
  let request = http.Request(
    method: http.Get,
    headers: [auth_header],
    ..http.default_request("https://esimerkki.com")
  )

  try response = elli.send(request)
  io.println(response)
}
```

Esimerkkitulostus:
```
Response(200, "OK", ...)
```

## Deep Dive - Syväsukellus:
HTTP-perusautentikaatio on alkeellinen tapa suojata verkkoresursseja. Se on osa HTTP-protokollaa vuodesta 1996. Vaikka se on yksinkertainen, se ei ole turvallisin vaihtoehto - käytä HTTPS:n yli.

Vaihtoehtoja ovat OAuth tai API-avaimet. Ne tarjoavat kehittyneempää suojaa, mutta ne vaativat enemmän asetuksia.

Kun lähetät HTTP-pyynnön Gleamilla, voit käyttää `http` kirjaston toimintoja. Muista koodata käyttäjätunnus ja salasana Base64-koodauksella. `elli` on eräs Gleamin tarjoama HTTP-asiakas, jolla pyynnöt lähetetään.

## See Also - Katso Myös:
- Gleam HTTP library documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Elli, a Gleam HTTP client: [https://github.com/gleam-experiments/elli](https://github.com/gleam-experiments/elli)
- HTTP authentication: Basic and Digest Access Authentication: [https://tools.ietf.org/html/rfc2617](https://tools.ietf.org/html/rfc2617)
- Web security topics on MDN: [https://developer.mozilla.org/en-US/docs/Web/Security](https://developer.mozilla.org/en-US/docs/Web/Security)