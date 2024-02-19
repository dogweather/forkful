---
aliases:
- /fi/python/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:18.211429-07:00
description: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla tarkoittaa\
  \ k\xE4ytt\xE4j\xE4tilin tunnistetietojen (yleens\xE4 k\xE4ytt\xE4j\xE4nimi ja salasana)\
  \ l\xE4hett\xE4mist\xE4\u2026"
lastmod: 2024-02-18 23:09:07.183366
model: gpt-4-1106-preview
summary: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla tarkoittaa k\xE4\
  ytt\xE4j\xE4tilin tunnistetietojen (yleens\xE4 k\xE4ytt\xE4j\xE4nimi ja salasana)\
  \ l\xE4hett\xE4mist\xE4\u2026"
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTTP-pyynnön lähettäminen perusautentikoinnilla tarkoittaa käyttäjätilin tunnistetietojen (yleensä käyttäjänimi ja salasana) lähettämistä verkkopalvelimelle pääsyn saamiseksi. Ohjelmoijat käyttävät tätä menetelmää turvatakseen resurssit ja päästäkseen käsiksi suojausta vaativiin web-resursseihin.

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
