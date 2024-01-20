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

## Mitä & Miksi?

HTTP-pyynnön lähettäminen on, kun ohjelma pyytää tietoa toiselta ohjelmalta tai palvelimelta verkon välityksellä. Ohjelmoijat tekevät tämän saadakseen tiedot, joita he tarvitsevat - esimerkiksi verkkosivupäivitykset, tiedostojen lataamiset, tai API:n käyttäminen.

## Näin se tehdään:

### Ensin asenna `curl`-paketti:
```Bash
sudo apt-get install curl
```

### Seuraavaksi voit lähettää HTTP GET -pyynnön seuraavasti:

```Bash
curl http://www.verkkosivusi.com
```
Tämä koodi lähettää pyynnön sivustoon "www.verkkosivusi.com" ja näyttää vastauksen.

### HTTP POST -pyynnön lähettäminen on yhtä helppoa:

```Bash
curl -d "param1=val1&param2=val2" -X POST http://www.verkkosivusi.com/login
```
Tämä lähetetään **/login** päätteiseen URL-osoitteeseen.

## Syvemmälle:

### Historiallinen konteksti
HTTP-pyyntöjen lähetys on ollut perusteknologia, jota käyttävät verkkosovellukset 1990-luvun alkupuolelta asti.

### Vaihtoehtoja
`wget` voi olla vaihtoehto `curl`:lle - se on toinen, perinteinen tapa lähettää HTTP-pyyntöjä.

### Yksityiskohtia
`curl` tukee lukuisia protokollia. Useita parametreja voidaan käyttää pyyntöjen mukauttamiseen.

## Katso myös:

- [HTTP Made Really Easy](http://www.jmarshall.com/easy/http/)
- [`man`-sivut `curl`](https://curl.haxx.se/docs/manpage.html)
- [`man`-sivut `wget`](https://www.gnu.org/software/wget/manual/wget.html)