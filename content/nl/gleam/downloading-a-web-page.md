---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:58:51.985658-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het ophalen van de inhoud ervan via HTTP. Programmeurs doen dit voor webscraping, data-analyse of om te interageren met webservices.

## Hoe:

Laten we een webpagina ophalen met Gleam met behulp van het `gleam_http` pakket. Ga ervan uit dat `gleam_http` en `gleam_otp` al in je projectafhankelijkheden staan.

```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() -> Result(String, Nil) {
  let response = httpc.send(http.Request(to: "http://example.com")) // Maak de GET-aanvraag
  should.equal(response.status, 200) // Bevestig dat de statuscode OK is
  Ok(response.body) // Geef de body van de reactie terug
}

```

Voorbeelduitvoer na het uitvoeren van je code kan er zo uitzien:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
```

## Diepgaande duik

Lang geleden, in de vroege dagen van het web, was het downloaden van een pagina zo simpel als telnetten naar poort 80. Vandaag de dag heb je bibliotheken en talen, zoals Gleam, die zorgen voor de ingewikkelde HTTP-details.

Alternatieven voor `gleam_http` zijn onder meer lager-niveau bibliotheken of het interface met andere Erlang-/Elixir-bibliotheken met behulp van Gleams interoperabiliteitsfuncties.

De `gleam_http` functie `httpc.send()` doet het zware werk in ons voorbeeld. Het is gebaseerd op de Erlang `httpc` module, wat zorgt voor een eenvoudige API met een vleugje van Gleams typeveiligheid en patroonherkenning.

## Zie ook

- Gleam-documentatie: https://hexdocs.pm/gleam/gleam_http/
- `gleam_http` GitHub repo: https://github.com/gleam-lang/http
- Een inleiding tot HTTP: https://developer.mozilla.org/nl/docs/Web/HTTP
- Voor een diepgaande blik op webscraping, bekijk Beautiful Soup voor Python: https://www.crummy.com/software/BeautifulSoup/
