---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyynnön lähettäminen perustunnistuksella tarkoittaa käyttäjänimen ja salasanan sisällyttämistä HTTP-pyynnöön autentikoinnin varmistamiseksi. Ohjelmoijat tekevät tämän tietoturvan parantamiseksi ja oikeiden käyttäjien varmistamiseksi.

## Miten tehdä:

Pythonissa voit tehdä tämän `requests`-moduulin avulla. Kokeile seuraavaa:

```Python
import requests
from requests.auth import HTTPBasicAuth

# Luo tunnistustiedot
credentials = HTTPBasicAuth('kayttajanimi', 'salasana')

# Lähetä pyyntö
response = requests.get('https://esimerkki.com', auth=credentials)

# Tulosta vastauksen status-koodi
print(response.status_code)
```

Tämän pitäisi tulostaa vastauksen status-koodi (esim. `200` onnistuneelle pyynnölle).

## Syvempi tarkastelu:

Alun perin HTTP Basic authentication otettiin käyttöön sellaisenaan, ja sitä on käytetty laajasti tähän päivään asti. Se on yksinkertainen ja suoraviivainen, mutta se ei ole turvallisin menetelmä.

Vaihtoehtona voit käyttää kerrostuksellisempia autentikointijärjestelmiä, kuten OAuth2 tai JWT-tunnukset. Nämä menetelmät lisäävät ylimääräisiä turvapiirejä, mikä tekee niistä turvallisemman vaihtoehdon.

Pythonin `requests`-kirjasto käyttää `Authorization`-otsaketta ja toimittaa perustunnistustiedot `Base64`-koodatut käyttäjänimi ja salasana.

## Katso myös:

- Requests-kirjaston dokumentointi: [https://docs.python-requests.org/en/latest/](https://docs.python-requests.org/en/latest/)
- HTTP Basic authenticationin määrittely: [https://datatracker.ietf.org/doc/html/rfc7617](https://datatracker.ietf.org/doc/html/rfc7617)
- Pythonin virallinen dokumentaatio: [https://www.python.org/doc/](https://www.python.org/doc/)