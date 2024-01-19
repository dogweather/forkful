---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä se on & Miksi?
HTTP-pyynnön lähettäminen tarkoittaa tiedonpyynnön tarjoamista palvelimelle, jotta tämä toimittaa tietoja selaimeen. Ohjelmoijat tekevät tämän kommunikoidakseen verkkopalvelinten kanssa ja hakea tai lähetä tietoa.

## Näin teet:
Pythonin requests-kirjasto tekee HTTP-pyyntöjen lähettämisestä helppoa. Tarkastele alla olevaa koodia:

```Python
import requests

# GET-pyyntö
response = requests.get("https://jsonplaceholder.typicode.com/posts")

# Tulosta vastaus
print(response.text)
```
Kun suoritat tämän koodin, saat JSON-muodossa olevan vastauksen.

## Syvempi sukellus
HTTP-pyynnön lähettäminen juontaa juurensa HTML-kielen ja internetin alkumetreiltä. Se on tärkeä osa "asiakas-palvelin"-mallin ymmärtämistä. 

Vaikka Pythonin `requests`-kirjasto on suosittu, on myös muita tapoja lähettää HTTP-pyyntöjä. Esimerkiksi `http.client`-moduuli tarjoaa alhaisemman tason käyttöliittymän, joka saattaa olla hyödyllinen erityistapauksissa.

On hyvä ymmärtää, että HTTP-pyyntö voi sisältää useita osia: menetelmän (GET, POST, jne.), URL:n, otsakkeita ja joissakin tapauksissa tietojenkäsittelyn (kuten JSON).

## Katso myös:
Seuraavista lähteistä saat lisätietoa:
- Python requests -kirjaston dokumentaatio: https://requests.readthedocs.io/
- HTTP:n yleinen johdatus: https://developer.mozilla.org/fi/docs/Web/HTTP/Overview
- Pythonin http.client-moduulin dokumentaatio: https://docs.python.org/3/library/http.client.html