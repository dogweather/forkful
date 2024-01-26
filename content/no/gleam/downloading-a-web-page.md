---
title:                "Nedlasting av en nettside"
date:                  2024-01-20T17:44:15.685569-07:00
model:                 gpt-4-1106-preview
simple_title:         "Nedlasting av en nettside"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
"## Hva & Hvorfor?"

Å laste ned en nettside betyr å hente ned innholdet for å lese eller behandle det. Programmerere gjør dette for å samle data, sjekke tilgjengelighet eller integrere med andre tjenester.

## How to:
"## Hvordan gjøre det:"

```gleam
import gleam/http
import gleam/httpc
import gleam/should

pub fn main() {
  let response = httpc.send(http.Request(
    method: http.Get,
    url: "https://eksempelside.no",
    headers: [],
    body: http.Body(None),
  ))
  
  assert Ok(response) = response
  should.equal(response.status, 200)
  should.contain(response.body, "Velkommen til vår hjemmeside!")
}
```
Eksempel på output:
```
OK (status 200)
"Innholdet på nettsiden her... Velkommen til vår hjemmeside!"
```

## Deep Dive:
"## Dypdykk:"

I tidligere dager var det å laste ned nettsider som regel gjort med verktøy som `curl` eller `wget` i terminalen, eller ved å bruke biblioteker som `libcurl` i programmeringsspråk. Alternativer for Gleam kan være direkte HTTP-klienter som `httpc` eller bruk av eksterne biblioteker som wrapper rundt disse verktøyene for tilleggsfunksjonalitet.

Selv om å laste ned en nettside kan virke rett frem, er det komplekse detaljer å vurdere, som håndtering av HTTP-headers for å lede om eller håndtere informasjonskapsler (cookies), og feilhåndtering for nettverksproblemer eller ugyldige svar.

## See Also:
"## Se også:"

- HTTP klient verktøy som Curl: [Curl](https://curl.se/)
- Nettverksprogrammeringsguide for nybegynnere: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Learn/Common_questions/How_does_the_Internet_work)
