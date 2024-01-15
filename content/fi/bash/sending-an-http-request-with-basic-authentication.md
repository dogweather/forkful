---
title:                "Perusautentikoinnin lähettäminen http-pyynnöllä"
html_title:           "Bash: Perusautentikoinnin lähettäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnöllä"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Ison luokan verkkosivustot käyttävät usein autentikaatiota varmistaakseen, että vain oikeilla käyttäjillä on pääsy tiettyihin osiin sivustosta. Tämä tarkoittaa, että sinun täytyy olla täysin tunnistetun käyttäjän lähettäessäsi HTTP-pyynnön tällaiseen sivustoon. Tämä artikkeli näyttää sinulle, miten voit tehdä sen Bashilla käyttäen perustason autentikaatiota.

## Kuinka

Autentikointiprosessi sisältää yleensä käyttäjän tietojen lähettämisen salattuna sivuston palvelimelle. Tämä tapahtuu tavallisesti HTTP-pyynnön otsakkeessa lähetetyn "Authorization" -kentän kautta. Alla on esimerkkejä siitä, miten voit lähettää perustason autentikaatiopäällikön käyttäen "curl" -komennolla. Korvaa `-u` -parametrin arvot omilla käyttäjätiedoillasi.

```Bash
# Autentikointi käyttäen käyttäjänimeä ja salasanaa
curl -u username:password URL
```

```Bash
# Autentikointi käyttäen käyttäjänimeä ja tyhjää salasanaa
curl -u username: URL
```

Kun oikeat tiedot on lähetetty, saat vastauksena pyynnön onnistuneen lähetyksen ilmoituksen.

```
Enter host password for user 'username':
<html><body><h1>It works!</h1></body></html>
```

## Syvällinen katsaus

Perustason autentikaatio perustuu HTTP-pyynnön otsakkeisiin, erityisesti "Authorization" -kenttään, joka sisältää käyttäjän tietojen salatun version. Autentikaatioprosessi voidaan myös toteuttaa käsin, lähettämällä "Authorization" -kenttä itse. Tämä vaatii tarkan koodin luomista ja ymmärrystä perustason autentikaation toiminnasta.

## Katso myös

- [curl manuaali](https://curl.se/docs/manpage.html)
- [HTTP-perusautentikointi](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)
- [Linux Bashin perusteet](https://wiki.ubuntu-fi.org/Bashin_perusteet)