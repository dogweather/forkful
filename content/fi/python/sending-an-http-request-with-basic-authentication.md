---
title:                "Perusautentikoinnilla http-pyynnön lähettäminen"
html_title:           "Python: Perusautentikoinnilla http-pyynnön lähettäminen"
simple_title:         "Perusautentikoinnilla http-pyynnön lähettäminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi lähettää HTTP-pyyntö käyttäjätunnuksella ja salasanalla? Monissa tapauksissa palveluntarjoajat vaativat käyttäjiä todentamaan tunnistetietonsa ennen kuin he saavat pääsyn tiettyihin resursseihin. Tämä tapahtuu yleensä lähettämällä HTTP-pyyntö käyttäjätunnuksella ja salasanalla, jotta käyttäjä voidaan tunnistaa ja tarvittaessa valtuuttaa.

## Kuinka

```Python
import requests

# Määritä pyyntö parametrit
url = "https://www.example.com/api"
username = "käyttäjätunnus"
password = "salasana"

# Luo autentikointitiedot käyttäjätunnuksella ja salasanalla
auth = (username, password)

# Lähetä HTTP-pyyntö autentikointitiedoilla
response = requests.get(url, auth=auth)

# Tulosta vastauksen statuskoodi
print(response.status_code)
```
```
200
```

```Python
import requests
import base64

# Määritä pyyntö parametrit
url = "https://www.example.com/api"
username = "käyttäjätunnus"
password = "salasana"

# Luo autentikointistringi
auth_string = username + ":" + password

# Muunna autentikointistringi base64-muotoon
base64_auth_string = base64.b64encode(auth_string.encode("utf-8"))

# Luo otsikkoparametrit
headers = {
    "Authorization": "Basic " + base64_auth_string.decode("utf-8")
}

# Lähetä HTTP-pyyntö otsikkoparametreilla
response = requests.get(url, headers=headers)

# Tulosta vastauksen statuskoodi
print(response.status_code)
```
```
200
```

## Syväsukellus

Basic-authentikaatio on yksi yleisimmistä tapoista autentikoida käyttäjät web-sovelluksissa. Se perustuu käyttäjän lähettämään käyttäjätunnukseen ja salasanaan, jotka koodataan base64-muotoon ja lähetetään HTTP-pyynnön otsikkoparametreina tai autentikointitietoina. On tärkeää varmistaa, että käyttäjätunnus ja salasana eivät ole avoimesti nähtävillä, ja salaaminen base64-muotoon ei tarjoa täydellistä turvallisuutta.

## Katso myös

- [Python Requests -dokumentaatio](https://2.python-requests.org/en/master/)
- [Base64 -moduulin dokumentaatio](https://docs.python.org/3/library/base64.html)
- [HTTP-pyyntö ja vastaus -tiedot W3Schoolsissa](https://www.w3schools.com/python/module_requests.asp)