---
title:                "Å sende en HTTP-forespørsel"
aliases:
- /no/bash/sending-an-http-request/
date:                  2024-01-20T17:58:57.790080-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel går ut på å be om data eller handling fra en server. Programmerere gjør dette for å hente informasjon, interagere med webtjenester eller APIer.

## Slik gjør du det:

For å sende en HTTP-forespørsel kan du bruke `curl` eller `wget`. Disse kommandoene er kraftige og vanlige verktøy som er forhåndsinstallert på mange Unix-systemer.

```Bash
# Bruk curl for å sende en GET-forespørsel
curl http://example.com

# Send en POST-forespørsel med curl 
curl -X POST http://example.com/api/data -d '{"key1":"value1","key2":"value2"}' -H "Content-Type: application/json"

# Last ned en fil med wget
wget http://example.com/fil.zip
```

Når du kjører disse kommandoene, vil du se serverens respons direkte i terminalen.

## Dypdykk:

HTTP-forespørslene har vært hjertet av webkommunikasjon siden tidlig på 90-tallet. De lar klienter og servere utveksle informasjon gjennom kjente metoder som `GET` og `POST`.

Alternativer til `curl` og `wget` inkluderer verktøy som `httpie` eller programmeringsspråk som Python med biblioteker som `requests`.

Når du implementerer en HTTP-forespørsel:

- **GET** brukes for å hente data.
- **POST** brukes for å sende data til serveren.
- Det er også andre metoder som `PUT`, `DELETE`, `PATCH` osv., avhengig av handlingen du vil utføre.

HTTP-forespørsler kan ha ulike headerfelt som gir ytterligere kontekst til serveren, som `Content-Type` eller autentiseringsinformasjon.

## Se også:

- `curl` offisiell dokumentasjon: https://curl.se/docs/
- `wget` manuellsider: https://www.gnu.org/software/wget/manual/wget.html
- HTTPie: https://httpie.io/
- Python `requests` bibliotek: https://docs.python-requests.org/

Her kan du utforske og eksperimentere mer med forskjellige typer forespørsler, opsjoner og avanserte scenarier.
