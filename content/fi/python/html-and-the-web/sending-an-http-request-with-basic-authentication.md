---
date: 2024-01-20 18:02:18.211429-07:00
description: 'Kuinka toimitaan: .'
lastmod: '2024-03-13T22:44:56.143269-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

## Kuinka toimitaan:
```Python
import requests
from requests.auth import HTTPBasicAuth

url = 'https://example.com/api/data'
username = 'kayttaja'
password = 'salasana'

vastaus = requests.get(url, auth=HTTPBasicAuth(username, password))

if vastaus.status_code == 200:
    print('Pyynnön onnistui:', vastaus.text)
else:
    print('Pyynnön epäonnistui, statuskoodi:', vastaus.status_code)
```

## Syväsukellus:
Perusautentikointi HTTP-pyynnöissä on yksi vanhimmista menetelmistä verkkoresurssien suojaamiseen; se sisällytettiin jo HTTP/1.0-spesifikaatioon 1990-luvun alussa. Modernit vaihtoehdot, kuten OAuth ja JWT (JSON Web Token), tarjoavat parempaa turvallisuutta ja joustavuutta. Perusautentikoinnin toteutuksessa käyttäjätunnus ja salasana enkoodataan base64-muotoon ja lähetetään osana `Authorization`-otsaketta. Vaikka yksinkertainen, menetelmä on haavoittuvainen salakuuntelulle ja hyökkäyksille, ellei sitä käytetä salatulla yhteydellä (kuten HTTPS).

## Katso myös:
- Python `requests` kirjaston dokumentaatio: https://docs.python-requests.org/
- HTTP-perusautentikoinnin yleistietoa: https://en.wikipedia.org/wiki/Basic_access_authentication
- Tietoturvaan liittyen, suositeltavaa on tutustua tutkimuksiin OAuthista: https://oauth.net/2/
- JWT (JSON Web Tokens): https://jwt.io/introduction/
