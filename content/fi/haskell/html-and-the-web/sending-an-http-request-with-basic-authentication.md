---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 18:01:36.556352-07:00
description: "Kuinka: Tarvitset `http-conduit`-paketin HTTP-toimintoihin ja `base64-bytestring`-paketin\
  \ tunnistetietojen koodaamiseen. Tuo ne ja k\xE4yt\xE4\u2026"
lastmod: '2024-04-04T00:26:53.591517-06:00'
model: gpt-4-0125-preview
summary: Tarvitset `http-conduit`-paketin HTTP-toimintoihin ja `base64-bytestring`-paketin
  tunnistetietojen koodaamiseen.
title: "L\xE4hett\xE4m\xE4ss\xE4 HTTP-pyynt\xF6\xE4 perusautentikoinnilla"
weight: 45
---

## Kuinka:
Tarvitset `http-conduit`-paketin HTTP-toimintoihin ja `base64-bytestring`-paketin tunnistetietojen koodaamiseen. Tuo ne ja käytä `applyBasicAuth`-funktiota lisätäksesi tunnistetiedot pyyntöösi.

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)
import Data.ByteString.Base64 (encode)

-- Rakenna perusautentikointiotsikko
let username = "user"
let password = "pass"
let auth = encode $ pack (username ++ ":" ++ password)

-- Luo pyyntösi
request' = parseRequest_ "GET http://example.com/secret"
let request = setRequestHeader "Authorization" ["Basic " <> auth] request'

-- Suorita pyyntö
response <- httpLBS request

-- Käsittele vastaus
print $ getResponseBody response
```

Tämä tulostaa API-vastauksen, jos tunnistetietosi ovat oikein.

## Syventävä tarkastelu
Perusautentikointi on muinaishistoriaa verkkovuosissa; se suunniteltiin 90-luvun alussa, ja se on niin yksinkertainen kuin olla ja voi: base64-koodattu `käyttäjänimi:salasana` lähetetään otsikossa. Siitä puuttuvat hienot ominaisuudet kuten poletin vanheneminen ja, koska se on salaamaton, sitä tulisi aina käyttää HTTPS-yhteyden yli.

Vaihtoehdot, kuten OAuth, tarjoavat turvallisemman, yksityiskohtaisemman hallinnan. Haskellille kirjastot kuten `http-client` ja `wreq` tarjoavat enemmän vaihtoehtoja ja joustavuutta.

Toteutuksessa muista, että tunnistetietoja ei pidä kovakoodata! Käytä tuotannossa ympäristömuuttujia tai turvallista holvia. Ja koska `base64`-koodaus ei ole salausta (kuka tahansa voi purkaa sen), HTTPS ei ole vain hyvä idea, se on välttämätön.

## Katso myös
- Haskell `http-conduit` dokumentaatio: https://hackage.haskell.org/package/http-conduit
- `base64-bytestring` koodausta varten: https://hackage.haskell.org/package/base64-bytestring
- Tiukkaan turvallisuuteen, lue Haskellissa OAuth2:sta: https://hackage.haskell.org/package/hoauth2
- Lue parhaista käytännöistä salaisuuksien säilyttämiseen: https://www.yesodweb.com/book/security-considerations
