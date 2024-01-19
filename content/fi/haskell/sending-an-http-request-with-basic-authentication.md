---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tapa kommunikoida verkkopalvelimien kanssa, antaen ne meille tietoa tai hallinnoiden niitä. Se on perusedellytys useimmille web-ohjelmistoille ja -sovelluksille.

## Kuinka:

Voit lähettää HTTP-pyynnön käyttäen perusautentikointia Haskellissa Wreq-kirjaston avulla. Tässä on pieni esimerkki:

```Haskell
import Network.Wreq

opts = defaults & auth ?~ basicAuth (toS "username") (toS "password")

main = do
  r <- getWith opts "http://example.com"
  print r
  ```
Tämä ohjelma asettaa HTTP-pyynnön perusautentikoinnin ja huhuilee sen `http://example.com` -sivustoon. Tulos tulostetaan konsoliin.

## Syvällisempi tutkimus:

Perusautentikointi määriteltiin ensimmäisen kerran HTTP/1.0 -standardissa ja se on ollut olennainen osa HTTP-protokollaa siitä lähtien. Muista, että perusautentikointi lähettää tunnukset Base64-koodattuna, mikä ei ole turvallista.

Haskell-ohjelmoijana voit myös harkita muita autentikointimenetelmiä. Esimerkiksi Bearer-token-autentikointi, OAuth tai digest-autentikointi, jotka tarjoavat paremman turvallisuuden.

Tässä esimerkissä käytämme Wreq-kirjastoa, mutta Haskellin HTTP-klientti -kirjastoa voidaan myös käyttää. Kirjastosta riippumatta HTTP-pyynnön lähettäminen perusautentikoinnilla noudattaa suurelta osin samoja askeleita.

## Katso myös:

1. [Wreq-kirjaston dokumentaatio](https://hackage.haskell.org/package/wreq) 
2. [HTTP-klientti -kirjaston dokumentaatio](http://hackage.haskell.org/package/http-client)
3. [HTTP-Autentikointi: Perus- ja Digest -tunnistusmenetelmät - dokumentaatio](https://tools.ietf.org/html/rfc2617)
4. [OAuth 2.0 protokolla](https://tools.ietf.org/html/rfc6749)