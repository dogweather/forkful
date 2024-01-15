---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
HTTP-pyynnön lähettäminen on tärkeä taito monille ohjelmoijille ja web-kehittäjille, koska se mahdollistaa tiedon hakemisen ja lähettämisen verkkosivuilta ja sovelluksilta.

## Kuinka tehdä
```Bash
# Lähetä GET-pyyntö
curl www.example.com

# Lähetä POST-pyyntö
curl -X POST -d "parametri=arvo" www.example.com

# Lähetä mukautettu HTTP-pyyntöotsikko
curl -H "Otsikko: arvo" www.example.com

# Tallenna vastaus tiedostoon
curl -o tiedosto.html www.example.com

# Suorita JavaScript-koodi pyynnön kanssa
curl -s https://www.example.com | grep "hakusanat"
```

```Bash
# Esimerkkitulos GET-pyynnöstä
<!DOCTYPE html>
<html>
<head>
  <title>Esimerkki</title>
</head>
<body>
  <h1>Tervetuloa</h1>
  <p>Tämä on esimerkkisivu.</p>
</body>
</html>

# Pyynnön mukana lähetetty vastaus
URL: www.example.com
HTTP-metodi: GET
Statuskoodi: 200 OK
```

## Syvemmälle
HTTP-pyynnöt ovat tärkeä osa web-kehittämistä ja käytetään usein erilaisten APIen ja palveluiden integroimiseen sovelluksiin. Niissä on myös vaihtoehtoja, jotka mahdollistavat mukautettujen pyyntöjen lähettämisen, kuten erilaisten HTTP-metodien käyttäminen ja pyyntöjen automatisointi bash-skripteillä.

## Katso myös
- [Curl-komentojen dokumentaatio](https://curl.haxx.se/docs/manpage.html)
- [HTTP-opetusohjelma](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)
- [Postman-työkalu HTTP-pyyntöjen testaamiseen](https://www.postman.com/)