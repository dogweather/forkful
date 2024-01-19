---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel er en måte en datamaskin kommuniserer med en annen over internett, hovedsakelig for å be om data. Programmerere gjør dette for å bruke eller manipulere informasjon fra tredjepartstjenester.

## Hvordan gjøre det:

Å sende HTTP-forespørsler er en rett frem prosess i Bash ved hjelp av `curl`. For eksempel for å sende en GET-forespørsel:

```Bash
curl https://eksempel.no
```

Dette vil sende en GET-forespørsel til eksempel.no og utskriften vil være innholdet på siden.

For å sende en POST-forespørsel:

```Bash
curl -d "kropp=innhold" -X POST https://eksempel.no
```

Her sendes en POST-forespørsel med kroppen "kropp=innhold" til eksempel.no.

## Dypdykk

Siden tidlig på 90-tallet har HTTP-forespørsler vært grunnlaget for websurfing, da hver URL-klikk utløser en forespørsel. Alternativer til bruk av `curl` i Bash er `wget` og `httpie`, som har noe varierende funksjonalitet og syntaks. Hverken Bash eller disse verktøyene håndterer HTTP-forespørsler selv, de bruker biblioteker som `libcurl` eller `openssl`.

## Se Også

For enda mer inngående detaljer om `curl`, sjekk ut manualen deres [her](https://curl.se/docs/manual.html). Hvis du er interessert i det historiske aspektet av HTTP, kan du sjekke ut denne [linken](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview). For mer om alternativene `wget` og `httpie`, se [her](https://www.gnu.org/software/wget/) og [her](https://httpie.io/docs/v1.0.0/).