---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Bash: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

MITÄ & MIKSI?

HTTP-pyynnöt ja perusautentikointi -varmenteen lähettäminen on yksinkertainen tapa lähettää salattua tietoa palvelimelle. Ohjelmoijat käyttävät tätä tekniikkaa esimerkiksi käyttäjätunnusten ja salasanojen lähettämiseen palvelimelle tai suojatun sivuston käyttämiseen.

MITEN?

Voit lähettää HTTP-pyynnön perusautentikoinnilla käyttämällä `curl` komentoa Bashissa. Seuraavassa esimerkissä lähetämme pyynnön GitHubin API:lle käyttäen käyttäjätunnusta ja salasanaa:
```
curl -u käyttäjätunnus:salasana https://api.github.com/user
```
Tämän komennon pitäisi tulostaa käyttäjän tiedot, jos tunnistautuminen onnistui.

SYVÄDYYKKAUS

HTTP-pyynnöt ja perusautentikointi ovat olleet käytössä jo 90-luvulta lähtien. Basic-varmenteen lähettämisen sijaan voit käyttää myös Digest-varmennetta, joka tarjoaa paremman tietoturvan mutta on myös monimutkaisempi implementoida. Voit myös lähettää varmenteen käyttämällä Base64-koodausta, mutta tämä ei ole yhtä turvallinen kuin HTTPS-yhteys.

LISÄLUESTA

Lisätietoja HTTP-pyynnöistä ja perusautentikoinnista löydät seuraavista lähteistä:
- [HTTP-pyynnöt - W3Schools](https://www.w3schools.com/tags/ref_httpmethods.asp)
- [Perusautentikointi - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [cURL Dokuwiki - Basic Authentication](https://curl.haxx.se/docs/httpscripting.html#BasicAuthentication)