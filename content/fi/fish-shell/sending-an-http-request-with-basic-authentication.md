---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Fish Shell: HTTP-pyyntöjen lähettäminen perustodentamisen kanssa

## Mikä & Miksi?
HTTP-pyyntö perustodentamisella on menetelmä, jossa lähetetään tietoja verkon yli salasanan ja käyttäjänimen yhdistelmällä. Ohjelmoijat tekevät tämän tietoturvan kannalta olennaisen tiedon suojaamiseksi.

## Miten
```Fish Shell
# Lähetetään GET-pyyntö

set kai 'https://website.com'
set user 'username'
set pass 'password'

curl -u $user:$pass $kai
```     
Tämän pitäisi tuottaa tuloste, joka vastaa palvelimen vastausta. Voit jakaa sen helposti seuraavalla tavalla:

```Fish Shell
curl -s -u $user:$pass $kai | json_pp
```
Lähettääksesi esimerkiksi POST-pyynnön, käytät seuraavaa:

```Fish Shell
curl -d "param1=value1&param2=value2" -X POST -u $user:$pass $kai
```

## Syventävä sukellus
Perustodentaminen on ollut HTTP-protokollan osa sen alkupäiviltä lähtien. Vaikka se ei olekaan yhtä turvallinen kuin nykypäivän pitkälle kehitetyt menetelmät, kuten OAuth, se on edelleen yksinkertainen ja tehokas tapa suojata tietoja.

Vaihtoehtoja perustodentamiselle ovat muun muassa digitaalinen allekirjoitus (jossa verkkopyyntö signeerataan yksityisellä avaimella) tai token-pohjainen todentaminen.

Fish Shellin toteutus perustuu curl-ohjelmistolle, yhdelle laajimmin käytetylle verkkokirjastoille.

## Katso myös
- [Curl-manuaali ja -ohjeet](https://curl.se/docs/manual.html)
- [Fish Shellin kotisivu](https://fishshell.com/)
- [HTTP:n perustodentamisen yksityiskohtainen kuvaus](https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication)