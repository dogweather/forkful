---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside er prosessen med å hente data fra en internettserver og lagre den på brukerens enhet. Programmerere gjør dette for å kunne analysere, manipulere og hente data fra nettsiden for videre bruk.

## Hvordan:

Her er en grunnleggende kodeeksempel som bruker `requests` biblioteket i Python for å hente data fra en nettside:

```Python
import requests

url = 'http://www.google.no'
response = requests.get(url)

print(response.text)
```

Output:

```Python
<!doctype html>...
...Google-linkene
</html>
```

## Dypdykk:

Historisk sett har nedlastning av nettsider blitt brukt til mange formål, inkludert data mining, backup av nettsteder og offline lesning. Men, det er alltid viktig å overholde nettstedets robots.txt-fil og servicevilkår for å unngå å være på feil side av loven.

Alternativer for `requests` biblioteket inkluderer `httplib`, `treq`, og `http.client`; men `requests` forblir det mest populære valget på grunn av dets brukervennlighet og rike funksjoner.

Nedlastningen skjer over HTTP eller HTTPS protokoller. Når en GET forespørsel er gjort, serveren svarer med data som da blir lest av Python-koden.

## Se Også:

- [Python Requests Dokumentasjon](https://docs.python-requests.org/en/latest/)
- [W3School: Python Requests Tutorial](https://www.w3schools.com/python/ref_requests_get.asp)
- [Mozilla Developer Network HTTP Oversikt](https://developer.mozilla.org/nb/docs/Web/HTTP/Overview)

Husk at informatisk etikk og databehandlingslover bør alltid overholdes når du laster ned og behandler data fra internett.