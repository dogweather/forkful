---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:13.710660-07:00
description: 'Hoe: Zo laat je Python communiceren met een server met Basic Auth.'
lastmod: '2024-03-13T22:44:50.376372-06:00'
model: gpt-4-0125-preview
summary: Zo laat je Python communiceren met een server met Basic Auth.
title: Een HTTP-verzoek verzenden met basisauthenticatie
weight: 45
---

## Hoe:
Zo laat je Python communiceren met een server met Basic Auth.

```Python
import requests
from requests.auth import HTTPBasicAuth

# Vervang dit door je daadwerkelijke inloggegevens en het API-eindpunt waar je een verzoek naar doet
gebruikersnaam = 'cooluser'
wachtwoord = 'supergeheimpaswoord'
url = 'https://api.someservice.com/data'

response = requests.get(url, auth=HTTPBasicAuth(gebruikersnaam, wachtwoord))

# Bekijk wat we terugkrijgen
print(response.status_code)
print(response.json())  # uitgaande dat de respons in JSON-formaat is
```

De uitvoer kan er zo uitzien als alles goed gaat:

```
200
{'data': 'Je geheime spullen!'}
```

Maar als je de inloggegevens verprutst:

```
401
```

Dat is een niet-toegangsbord daar.

## Diepgaande Duik
Historisch gezien is HTTP Basic Auth zo old-school als maar kan voor webbeveiliging, een eenvoudige manier om de geheime handdruk met een website te doen. Het is niet erg veilig op zichzelf omdat het inloggegevens in platte tekst verzendt, slechts base64 gecodeerd - niet versleuteld. Gebruik altijd HTTPS om te voorkomen dat het grijpen van de inloggegevens net zo eenvoudig is als snoep van een baby pakken.

Er zijn veiligere alternatieven, zoals Digest Access Authentication waarbij het wachtwoord nooit openlijk over het netwerk wordt verzonden. OAuth is nog een grote, vooral voor API's vandaag de dag. Het is meer als het uitgeven van een tijdelijke VIP-pas dan elke keer ID tonen.

Onder de motorkap codeert de `requests`-bibliotheek je gebruikersnaam en wachtwoord en plakt deze in een `Authorization` header geformatteerd als `Basic base64encodedcredentials`. De server decodeert deze header, controleert je inloggegevens en als je legitiem bent, geeft het je toegang.

## Zie Ook
- De officiële documentatie van de `requests` bibliotheek geeft je de details over authenticatie en meer: https://docs.python-requests.org/en/latest/
- `http.client` voor wie zonder een bibliotheek van derden wil werken: https://docs.python.org/3/library/http.client.html
- Real Python duikt in de HTTP-basics en Python: https://realpython.com/python-requests/
