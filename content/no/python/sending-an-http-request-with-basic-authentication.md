---
title:                "Sende en http-request med grunnleggende autentisering"
html_title:           "Python: Sende en http-request med grunnleggende autentisering"
simple_title:         "Sende en http-request med grunnleggende autentisering"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Sending av HTTP-forespørsler med basic authentication er en vanlig måte å verifisere identiteten din på når du kommuniserer med en server. Dette er spesielt viktig når du sender sensitive data, som passord eller personlig informasjon.

## Slik gjør du det
```python
import requests

# Definer brukernavn og passord
username = "brukernavn"
password = "passord"

# Sett opp basic authentication
auth = requests.auth.HTTPBasicAuth(username, password)

# Spesifiser URL og send request med authentication
url = "https://www.example.com/api/users"
response = requests.get(url, auth=auth)

# Sjekk statuskode for å se om requesten var vellykket
print(response.status_code)

# Skriv ut innholdet i response
print(response.text)
```

**Output:**
200<br>
```json
{
    "user_id": 123,
    "username": "brukernavn"
}
```

## Dypdykk
Når du sender en HTTP-request med basic authentication, må du inkludere brukernavn og passord i headeren til requesten. Dette sikrer at kun autoriserte brukere har tilgang til serveren. Det er viktig å merke seg at basic authentication sender brukernavnet og passordet som "åpen teksts", noe som kan være en sikkerhetsrisiko. Derfor bør du alltid bruke HTTPS-protokollen når du sender slike requests.

## Se også
- [Requests dokumentasjon](https://requests.readthedocs.io)
- [Hvordan lage en HTTP-request med Python](https://realpython.com/python-requests/)