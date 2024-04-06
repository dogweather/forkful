---
date: 2024-01-20 18:00:33.683038-07:00
description: "How to: (Kuinka tehd\xE4:) HTTP-pyynn\xF6t ovat perusta web-kommunikaatiolle,\
  \ ja ne ovat olleet siit\xE4 l\xE4htien, kun Tim Berners-Lee kehitti ensimm\xE4\
  isen HTTP-\u2026"
lastmod: '2024-04-05T22:51:10.300842-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) HTTP-pyynn\xF6t ovat perusta web-kommunikaatiolle, ja\
  \ ne ovat olleet siit\xE4 l\xE4htien, kun Tim Berners-Lee kehitti ensimm\xE4isen\
  \ HTTP-version vuonna 1989."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

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
