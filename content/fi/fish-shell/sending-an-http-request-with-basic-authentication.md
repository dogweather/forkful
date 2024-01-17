---
title:                "Perusautentikoinnin lähettäminen http-pyynnöllä"
html_title:           "Fish Shell: Perusautentikoinnin lähettäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyynnöllä"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lähettäminen HTTP-pyyntö perustuu todennukselle yksinkertaisen todennuksen kanssa on tapa lähettää salaiset käyttöoikeudet API:lle tai verkkosivustolle. Ohjelmoijat käyttävät tätä menetelmää saadakseen pääsyn suojattuihin resursseihin ja hallitakseen muiden käyttäjien oikeuksia.

## Miten:
```Fish Shell ...```
Esimerkiksi, jos haluat lähettää GET-pyynnön verkkosivustolle, joka vaatii perustodennusta, voit käyttää seuraavaa koodia:

```Fish Shell
# URL josta lähetetään pyyntö
set url https://www.example.com
# Käyttäjänimi ja salasana
set username "käyttäjänimi"
set password "salasana"
# Lähetä pyyntö käyttäen perustodennusta
curl -u $username:$password $url
```
Tämä pyyntö lähettää käyttäjänimen ja salasanan verkkosivustolle HTTP-otsakkeessa perustodennusta käyttäen.

## Syvemmälle:
Tekniikka perustodennuksen käyttämiseen HTTP-pyynnöissä kehitettiin alunperin 90-luvulla auttamaan SMTP-sähköpostipalvelimia tarkistamaan sähköpostiosoitteiden aitoutta. On olemassa muitakin tapoja lähettää todennettuja HTTP-pyyntöjä, kuten OAuth ja OpenID Connect. Fish Shell tekee tämän prosessin helpoksi tarjoamalla curl-komennon, joka voi käsitellä todennuksen automaattisesti.

## Katso myös:
- Fish Shellin virallinen sivusto: https://fishshell.com/
- Curl-komento: https://curl.se/
- HTTP-perustodennus: https://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.27