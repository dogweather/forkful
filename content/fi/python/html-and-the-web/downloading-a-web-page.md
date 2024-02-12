---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:44:43.398325-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Ladataan web-sivu Pythonilla tarkoittaa sen sisällön noutamista internetistä. Koodarit tekevät tätä datan analysointiin, sisällön keräämiseen tai varmuuskopiointiin.

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
