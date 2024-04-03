---
date: 2024-01-20 18:00:33.683038-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-03-13T22:44:56.140457-06:00'
model: gpt-4-1106-preview
summary: .
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
