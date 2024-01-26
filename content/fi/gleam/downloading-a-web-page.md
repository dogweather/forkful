---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:43:56.567173-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Ladataan verkkosivu, eli otetaan sivun sisältö ja tallennetaan se paikallisesti. Ohjelmoijat tekevät tämän tiedonkeruun, sisällönanalyysin tai testauksen vuoksi.

## How to:
Gleamilla voit ladata verkkosivun käyttäen `httpc`-kirjastoa. Tässä esimerkki:

```gleam
import gleam/http/httpc
import gleam/http/response
import gleam/should
import gleam/io

pub fn main() {
  let response = httpc.send(https://example.com)
  match response {
    Ok(response) -> io.print("Onnistui: \(response.body) ")
    Error(e) -> io.println("Virhe: \(e)")
  }
}
```

Sample output:

```
Onnistui: <html>...
```

## Deep Dive
Verkkosivun lataaminen on yksi verkko-ohjelmoinnin perustoiminnoista. Se on suoraviivaista: lähetetään HTTP-pyyntö, odotetaan vastausta ja käsitellään se. Tämä konsepti on pysynyt muuttumattomana, vaikka tekniikat ja kirjastot kehittyvät.

Vaihtoehtoja on monia, kuten `curl` komentoriviltä tai korkeamman tason abstraktiot, esimerkiksi `reqwest` Rust-kielessä. Gleamissa `httpc` on suora ja tyypitetty tapa tehdä HTTP-pyyntöjä, ja se perustuu Erlangin `httpc`-moduuliin.

Erot korkeamman ja matalamman tason kirjastojen välillä ovat kontrollin ja helppokäyttöisyyden välillä. Gleamin `httpc` antaa tiukan tyypityksen edut ja selkeän virheenkäsittelyn mutkattomasti.

## See Also
- Gleam HTTP client documentation: [https://hexdocs.pm/gleam_http/](https://hexdocs.pm/gleam_http/)
- Erlang `httpc` module: [http://erlang.org/doc/man/httpc.html](http://erlang.org/doc/man/httpc.html)
- HTTP basics: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
