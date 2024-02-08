---
title:                "HTTP-pyynnön lähettäminen"
aliases:
- fi/python/sending-an-http-request.md
date:                  2024-01-20T18:00:33.683038-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Lähetämme HTTP-pyyntöjä kommunikoidaksemme web-palvelimien kanssa. Koodarit tekevät sen hakeakseen tietoa, lähettääkseen dataa ja interaktoidakseen verkkopalveluiden kanssa.

## How to: (Kuinka tehdä:)
```Python
# Tarvittavat kirjastot
import requests

# Lähetetään GET-pyyntö
vastaus = requests.get('https://api.github.com')

# Tulostetaan statuskoodi
print(vastaus.status_code)  # 200

# Lähetetään POST-pyyntö
data = {'key': 'value'}
vastaus = requests.post('https://httpbin.org/post', json=data)

# Tulostetaan vastaus JSON-muodossa
print(vastaus.json())
```

## Deep Dive (Sukellus syvyyksiin):
HTTP-pyynnöt ovat perusta web-kommunikaatiolle, ja ne ovat olleet siitä lähtien, kun Tim Berners-Lee kehitti ensimmäisen HTTP-version vuonna 1989. Vaihtoehtoja `requests`-kirjastolle ovat esimerkiksi `http.client` vakio Python-kirjastossa sekä ulkoiset kirjastot kuten `aiohttp` asynkroniseen kommunikaatioon. Implementoinnin yksityiskohdat riippuvat pyynnön tyypistä (GET, POST, PUT, DELETE...) ja joskus tarvitaan lisäparametreja, kuten headers tai cookies.

## See Also (Katso myös):
- Requests-kirjaston dokumentaatio: https://requests.readthedocs.io/
- Pythonin virallinen HTTP-client ohjeistus: https://docs.python.org/3/library/http.client.html
- HTTP-protokollan RFC: https://tools.ietf.org/html/rfc2616
- RESTful API-opas: https://restfulapi.net/
