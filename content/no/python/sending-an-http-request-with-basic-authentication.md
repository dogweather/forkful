---
title:                "Python: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler med grunnleggende autentisering kan være nyttig når du trenger å sikre dataoverføring mellom klient og server. Dette kan være spesielt viktig i situasjoner der sensitiv informasjon blir overført, som ved innlogging på en nettside.

## Slik gjør du det

For å sende en HTTP-forespørsel med grunnleggende autentisering i Python, må du først importere "requests" biblioteket. Deretter kan du lage en forespørsel og legge til autentiseringsinformasjon ved hjelp av "auth" parameteren.

```python
import requests

response = requests.get("https://www.mittnettsted.com", auth=("brukernavn", "passord"))
print(response.text)
```

Denne kodesnutten vil sende en GET-forespørsel til nettsiden "www.mittnettsted.com" med brukernavn og passord som autentiseringsinformasjon. Sørg for å erstatte disse verdiene med dine egne.

## Dypdykk

Når man sender en HTTP-forespørsel med grunnleggende autentisering, blir brukernavn og passord-kombinasjonen sendt i klartekst. Dette er en svakhet med denne autentiseringsmetoden, da noen kunne fange denne informasjonen og få tilgang til brukerkontoen din. Det er derfor viktig å sørge for at nettstedet du sender forespørselen til har en sikker forbindelse (HTTPS). Du kan også bruke andre autentiseringsmetoder som OAuth for å øke sikkerheten.

## Se også

- [Dokumentasjon for "requests" biblioteket](https://requests.readthedocs.io/en/master/)
- [Informasjon om HTTP-forespørsler og autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)