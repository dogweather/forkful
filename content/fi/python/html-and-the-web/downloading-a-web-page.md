---
date: 2024-01-20 17:44:43.398325-07:00
description: "How to: (Kuinka tehd\xE4:) ."
lastmod: '2024-03-13T22:44:56.142391-06:00'
model: gpt-4-1106-preview
summary: .
title: Verkkosivun lataaminen
weight: 42
---

## How to: (Kuinka tehdä:)
```Python
import requests

# Web-sivun URL-osoite
url = 'http://www.example.com'

# Lähetetään GET-pyyntö ja tallennetaan vastaus muuttujaan
response = requests.get(url)

# Tarkistetaan onnistuiko pyyntö
if response.ok:
    # Tulostetaan sivun sisältö
    print(response.text)
else:
    print('Sivun lataaminen epäonnistui, virhekoodi:', response.status_code)
```

Esimerkkituloste:

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</head>
<body>
...
</body>
</html>
```

## Deep Dive (Syväsukellus):
Historiallisessa kontekstissa web-sivujen lataaminen on ollut yleistä web-skrapingin alkuaikoina. Alkujaan tehtiin pelkkiä HTTP-pyyntöjä ilman kirjastoja. Nykyisin `requests`-kirjasto on Pythonin suosituin HTTP-client-kirjasto sen selkeän syntaksin ja toiminnallisuuden vuoksi. 

Vaihtoehtoisia tapoja ladata sivuja Pythonissa ovat `urllib`-standardikirjaston moduulit tai kolmannen osapuolen kirjastot kuten `httpx`. 

`requests` käyttää sisäisesti `urllib3`, ja sen peruskäyttö on helppoa: tee pyyntö, tarkista vastaus, ja käsittele data. Monimutkaisemmissa tapauksissa voi tarvita evästeiden käsittelyä, session ylläpitoa, tai erilaisia autentikaatio- ja yhteysasetuksia.

## See Also (Katso Myös):
- Requests dokumentaatio: https://requests.readthedocs.io/
- Pythonin `urllib`: https://docs.python.org/3/library/urllib.html
- HTTPX dokumentaatio: https://www.python-httpx.org/
