---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?
Å sende en HTTP-forespørsel er en måte datamaskiner henter eller sender data på gjennom Internett. Programmers bruker dette til å hente web-sideinnhold, API-data, filnedlasting og mer.

---

## Hvordan å:

For å sende en HTTP-forespørsel i Python, kan du bruke `requests` biblioteket. Hvis det ikke er installert, kan du enkelt legge det til med pip: 

```Python
pip install requests
```

Når `requests` er installert, kan du bruke det til å sende en GET forespørsel som dette:

```Python
import requests

response = requests.get('https://www.google.com')
print(response.status_code)
```

I dette tilfellet vil `status_code` gi oss HTTP-statuskoden for forespørselen. For en vellykket GET-forespørsel, skal du se `200`.

---

## Dypdykk:
1. **Historisk kontekst**: HTTP-forespørsler ble først definert i 1991 som en del av HTTP-protokollen, som er grunnlaget for all datakommunikasjon på World Wide Web.

2. **Alternativer**: Selv om `requests` er det mest populære valget, er det flere alternativer tilgjengelige for å sende HTTP-forespørsler i Python. Noen av disse inkluderer `httplib`, `urllib` og `http.client`.

3. **Implementeringsdetaljer**: Når en forespørsel er sendt, venter klienten på et svar fra serveren. HTTP-protokollen definerer både strukturen på forespørsler og svar. 

---

## Se Også:

* HTTP-forespørsler med Python - [Offisiell dokumentasjon](https://docs.python-requests.org/en/latest/user/quickstart/)
* Forstå HTTP-forespørsler - [Mozilla-veiledning](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
* HTTP i Python - [W3Schools veiledning](https://www.w3schools.com/python/module_requests.asp)