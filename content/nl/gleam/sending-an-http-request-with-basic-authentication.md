---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
date:                  2024-01-28T22:07:55.754143-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat en waarom?

Een HTTP-verzoek sturen met basisauthenticatie betekent dat je een gebruikersnaam en wachtwoord aan je verzoek toevoegt, als een geheime handdruk om toegang te krijgen tot een webbron. Programmeurs doen dit om ervoor te zorgen dat alleen zij die het weten (d.w.z. met de juiste inloggegevens) erdoor komen.

## Hoe te:

In Gleam ga je de `gleam/http` bibliotheek gebruiken. Hier is een vereenvoudigd voorbeeld:

```gleam
import gleam/http.{Request, BasicAuth, get}

pub fn fetch_resource_with_basic_auth() {
  let auth = BasicAuth(
    username: "awesome_dev",
    password: "superSecret123",
  )
  let request = Request(
    method: Get,
    url: "https://api.example.com/protected",
    headers: [],
    body: None,
    basic_auth: Some(auth),
  )

  assert Ok(response) = get(request)
  response
}
```

Wat je zult zien als je het uitvoert:

```gleam
Ok(#{
  status: 200,
  headers: [...],
  body: "Hier zijn je geheime gegevens!"
})
```

Of, als de inloggegevens verkeerd zijn:

```gleam
Ok(#{
  status: 401,
  headers: [...],
  body: ""
})
```

## Diepgaande duik

Vroeger was basisauthenticatie een van de eerste methoden om webcommunicatie te beveiligen; het is als een oud hangslot - eenvoudig maar niet het meest veilig.

Alternatieven? Je hebt OAuth voor complexere scenario's, of token-gebaseerde auth voor een compromis tussen eenvoud en veiligheid.

Wat betreft de implementatie, stopt basisauth in HTTP een Base64 gecodeerde tekenreeks (je gebruikersnaam- en wachtwoordcombinatie) in een 'Authorization'-header. Het is niet versleuteld, vandaar dat het basis is en niet aanbevolen voor gevoelige zaken zonder ten minste HTTPS. Ook moet je geen inloggegevens hardcoderen in je code; gebruik omgevingsvariabelen of een veilige kluisdienst.

## Zie ook

Duik dieper in:

- [HTTP-authenticatie op MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Veilige opslag van app-geheimen](https://learn.microsoft.com/en-us/aspnet/core/security/app-secrets)
