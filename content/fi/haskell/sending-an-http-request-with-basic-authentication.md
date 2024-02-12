---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
aliases:
- fi/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:08.531738-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

HTTP-pyynnön lähettäminen perusautentikoinnilla tarkoittaa palvelimelle lähtevän pyynnön otsakkeeseen sisällytettävää käyttäjätunnusten todentamista. Ohjelmoijat käyttävät sitä suojatun sisällön käsittelyyn ilman monimutkaisempia autentikointimenetelmiä.

# Kuinka:

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Base64 (encode)
import Data.ByteString.Char8 (pack)

-- Käyttäjätunnuksesi ja salasanasi
credentials :: String
credentials = "kayttaja:salasana"

-- Muodosta 'Basic' autentikointiotsake
basicAuth :: ByteString
basicAuth = "Basic " <> (encode . pack $ credentials)

-- Valmistele ja lähetä pyyntö
makeRequest :: IO ()
makeRequest = do 
    let request = setRequestHeader "Authorization" [basicAuth]
                $ parseRequest_ "GET http://esimerkki.fi/suojattu/sisalto"
    response <- httpLBS request
    putStrLn $ "Statuskoodi: " ++ show (getResponseStatusCode response)
    putStrLn $ "Vastauksen runko: " ++ show (getResponseBodys response)

main :: IO ()
main = makeRequest
```

Esimerkkikoodi luo HTTP-pyynnön ja liittää siihen Basic-autentikoinnin käyttäen annettuja käyttäjätunnuksia ja salasanoja. Käyttäjätunnuksia ja salasanoja ei koskaan pidä säilyttää kovakoodattuna tuotannossa.

# Syväsukellus

Ennen HTTPS:n ja monimutkaisempien autentikointiprotokollien yleistymistä perusautentikointi oli yleinen tapa suojata verkkosisältö. Se on yhä käytössä, mutta turvallisuusriskien vuoksi sen käyttö on suositeltavaa rajoittaa suojatun yhteyden yli.

Vaihtoehtoja perusautentikoinnille ovat OAuth, JWT (JSON Web Token), ja muut täydellisempiä turvatoimia tarjoavat menetelmät. Nämä menetelmät mahdollistavat monimutkaisten sovellusten turvallisen käyttäjänhallinnan.

# Katso Myös

- HTTP:n perusautentikointi: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme
- Network.HTTP.Simple dokumentaatio: https://www.stackage.org/haddock/lts-18.18/http-conduit-2.3.8/Network-HTTP-Simple.html
- Turvallisuus ja autentikointi Haskellissa: https://wiki.haskell.org/Web/Literature#Authentication
