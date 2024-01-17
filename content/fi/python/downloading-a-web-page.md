---
title:                "Verkkosivun lataus"
html_title:           "Python: Verkkosivun lataus"
simple_title:         "Verkkosivun lataus"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Lataaminen verkkosivulta tarkoittaa sen sisällön kopioimista internetistä omalle tietokoneellesi. Ohjelmoijat tekevät tätä, jotta he voivat käyttää kyseistä sivustoa ja sen tietoja muiden ohjelmien kanssa.

## Miten:
```Python
import requests

# Lataa verkkosivu sivustolta
response = requests.get("https://www.esimerkkisivusto.com")

# Tulosta verkkosivun sisältö
print(response.text)
```

Tulostaa:

```html
<!DOCTYPE html>
<html>
<head>
<title>Esimerkkisivusto</title>
</head>
<body>
<h1>Tervetuloa Esimerkkisivustolle!</h1>
<p>Täältä löydät kaikenlaista hyödyllistä tietoa.</p>
</body>
</html>
```

## Syvällinen sukellus:
Lataaminen verkkosivulta oli aikoinaan yleistä näppäimistöllä ja hiirellä, mutta nykyään se tapahtuu useimmiten automaattisesti ohjelmallisesti. Pythonissa voit käyttää pakettia nimeltä "requests" ladataksesi verkkosivun. Jos etsit vaihtoehtoja, voit myös käyttää muita paketteja, kuten "urllib" tai "urllib2". Voit myös ladata verkkosivun käyttäen komentokehotetta tai työkalua nimeltä "curl". 

## Katso myös:
- [Requests-dokumentaatio](http://docs.python-requests.org/en/master/)
- [Urllib-dokumentaatio](https://docs.python.org/3/library/urllib.html)
- [Curl-dokumentaatio](https://curl.haxx.se/docs/)