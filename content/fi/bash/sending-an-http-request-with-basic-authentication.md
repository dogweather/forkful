---
title:                "Bash: Http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "Http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa, on tarvetta kommunikoida muiden palveluiden kanssa. Yksi tapa tehdä tämä on lähettää HTTP-pyyntöjä, jolloin saadaan vastaus halutusta palvelusta. Usein tällaisessa kommunikoinnissa tarvitaan myös autentikointia, ja tämän vuoksi tarvitaan HTTP-pyyntöjä, joissa on perustason autentikointi.

## Miten tehdä se

Bash on loistava työkalu lähettämään HTTP-pyynnöt ja tämä onnistuu helposti käyttämällä `curl`-komennolla. Voimme lisätä autentikoinnin käyttämällä `--user` parametria ja antamalla sille käyttäjänimemme ja salasanamme. Alla on yksinkertainen esimerkki, jossa lähetämme GET-pyynnön GitHubin API:in ja tulostetaan vastaus:

```Bash
curl -u käyttäjänimi:salasana https://api.github.com/users/käyttäjänimi
```

Tämä antaa meille vastauksen JSON-muodossa, josta voimme halutessamme käsitellä haluamiamme tietoja. Tässä tapauksessa tulostamme vain käyttäjämme nimen:

```Bash
Käyttäjänimi: "John Doe"
```

## Syvällisesti

HTTP-pyynnöt perustason autentikoinnilla lähettävät käyttäjänimen ja salasanan muodossa Base64-koodattuna. Tällainen autentikointi ei kuitenkaan ole kovin turvallista, sillä käyttäjänimi ja salasana voivat olla helppoja arvata tai ne voivat päätyä väärille käsille. Tästä syystä onkin suositeltavaa käyttää muita autentikointimenetelmiä, kuten OAuthia.

## Katso myös

- [Bashin virallinen dokumentaatio](https://www.gnu.org/software/bash/manual/)
- [Curlin dokumentaatio](https://curl.haxx.se/docs/)
- [GitHubin kehittäjädokumentaatio](https://developer.github.com/)