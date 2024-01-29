---
title:                "Een HTTP-verzoek verzenden"
date:                  2024-01-28T22:08:17.056649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek verzenden is hoe je programma vraagt om gegevens van ergens anders op het internet. Programmeurs doen dit om te communiceren met webservices, verse gegevens op te halen, of om externe API's in hun apps te integreren.

## Hoe:
In Gleam hebben we nog geen ingebouwde HTTP-client, dus laten we de `gleam_http` bibliotheek gebruiken. Hier is een eenvoudig GET-verzoek:

```gleam
import gleam/http.{Response, Error}
import gleam/httpc

pub fn main() -> Result(Response, Error) {
  // Stuur een GET-verzoek naar example.com
  httpc.send(httpc.Request(
    method: httpc.Get,
    url: "http://example.com",
    headers: [],
    body: httpc.Empty,
  ))
}
```

Dit uitvoeren zal een verzoek naar example.com sturen en de respons retourneren. Onthoud, je moet het resultaat verwerken om er daadwerkelijk gebruik van te kunnen maken.

## Diepgaande duik
Historisch gezien was het versturen van HTTP-verzoeken een taak voor gespecialiseerde tools zoals `curl` of bibliotheken in andere talen. Het is vrij nieuw dat talen zelf soepele HTTP-verzoekfunctionaliteiten incorporeren. Alternatieven voor het versturen van HTTP-verzoeken in Gleam omvatten externe bibliotheken zoals `gleam_http` en platformspecifieke bindingen.

Wat betreft de implementatie, zijn er twee delen: het construeren van het verzoek en het ontvangen van de respons. Verzoeken hebben methoden (GET, POST, etc.), URL's, headers, en lichamen terwijl reacties statuscodes, headers, en lichamen bij zich dragen.

Het type systeem en patroonmatching van Gleam schitteren hier, waardoor uitvoerige foutafhandeling en duidelijke parsing van reacties mogelijk zijn. Dit is niet zomaar data de leegte in sturen en hopen op het beste; het is een gecontroleerd, gestructureerd gesprek.

## Zie ook
- [Gleam's HTTP-documentatie](https://hexdocs.pm/gleam_httpc/)
- [HTTP RFC 7231](https://tools.ietf.org/html/rfc7231) voor de technische details over HTTP.
- [Gleam's standaardbibliotheek](https://hexdocs.pm/gleam_stdlib/) voor andere netwerkfuncties.
