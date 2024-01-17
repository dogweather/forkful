---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla."
html_title:           "Python: HTTP-pyynnön lähettäminen perusautentikoinnilla."
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla."
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTTP-pyynnön lähettäminen perusautentikoinnin avulla tarkoittaa sitä, että lähettäessämme pyynnön verkkosivulle, järjestelmä pyytää meiltä käyttäjätunnusta ja salasanaa ennen kuin se antaa meille pääsyn pyydettyyn sisältöön. Tätä tehdään yleensä tietoturvasyistä, jotta vain oikeilla käyttäjillä on pääsy tiettyihin sivustoihin tai palveluihin.

## Kuinka:

```Python
import requests

url = 'https://example.com' 
# Replace with the URL of the website or service you want to access

username = 'example_username'
# Replace with your own username 

password = 'example_password'
# Replace with your own password

# Sending a GET request with basic authentication 
response = requests.get(url, auth=(username, password))

# Accessing the response's status code
print(response.status_code) # Should output 200 if successful

# Accessing the response's content
print(response.content) # Will contain the website's HTML

# Sending a POST request with basic authentication
response = requests.post(url, auth=(username, password), json={'key': 'value'})

# Accessing the response's status code
print(response.status_code) # Should output 200 if successful

# Accessing the response's content
print(response.content) # Will contain the response from the server
```

## Syväsukellus:
Perusautentikointi kehitettiin alun perin HTTP-protokollaan, kun tarve muodostaa suojattuja yhteyksiä syntyi. Nykyään on olemassa myös muita tunnistautumismenetelmiä, kuten Digest-autentikointi, jotka tarjoavat parempaa tietoturvaa ja salauksen vaihtoehtona perusautentikoinnille. Perusautentikointi on kuitenkin edelleen yleisesti käytetty menetelmä, koska se on helppo toteuttaa ja tukee lähes kaikkia verkkopalveluita.

## Katso myös:
- [Pythonin virallinen Requests-kirjaston dokumentaatio](https://requests.readthedocs.io/en/latest/)
- [Information Security - Basic HTTP Authentication](https://www.infosecurity.nl/knowledge-item/N58395/Basic-HTTP-authentication)