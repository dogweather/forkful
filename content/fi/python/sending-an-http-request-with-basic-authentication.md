---
title:                "Python: Http-pyynnön lähettäminen perustason todennuksella"
simple_title:         "Http-pyynnön lähettäminen perustason todennuksella"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat lähettää HTTP-pyynnön ja varmistaa sen turvallisuuden, on tärkeää sisällyttää siihen basic authentication. Tämä auttaa pääsemään suojattuihin verkkoresursseihin, kuten salasanasuojattuihin verkkosivustoihin.

## Kuinka

```Python
import requests

url = "https://www.example.com/login"
user = "käyttäjänimi"
password = "salasana"

r = requests.get(url, auth=(user, password))
print(r.status_code)
print(r.text)
```

Koodiesimerkki näyttää, kuinka käyttäjänimi ja salasana voidaan sisällyttää HTTP-pyynnön autentikointitietoihin käyttäen requests-kirjastoa. Rivi "auth=(user, password)" lähettää basic authentication -todennustiedot pyynnön mukana.

Esimerkkitulostus:

```
200
<p>Welcome, user!</p>
```

2.11.0 ja uudemmissa versioissa requests-kirjastoa ei tarvitse erikseen asentaa, sillä se sisältyy jo Pythonin vakioasennukseen.

## Syvädykset

Basic authentication on yksi HTTP:n autentikointityypeistä ja se on yksi vanhimmista ja yksinkertaisimmista tavoista varmistaa verkkoresurssien turvallisuus. Autentikointitiedot lähetetään salattuna base64-muodossa.

Basic authenticationin suurin heikkous on se, että käyttäjänimi ja salasana lähetetään selkeäkielisinä pyynnön mukana. Tämä tekee siitä alttiin salasanan kaappaamiselle ja luvattomalle pääsylle verkkoresursseihin. Tämän vuoksi on suositeltavaa käyttää muita autentikointityyppejä, kuten token-pohjaista autentikointia.

## Katso myös

- [Requests-kirjaston dokumentaatio](https://requests.readthedocs.io/)
- [Basic authenticationin tietoturva](https://www.ietf.org/rfc/rfc2617.txt)
- [Token-pohjainen autentikointi Pythonilla](https://realpython.com/token-based-authentication-with-flask/)