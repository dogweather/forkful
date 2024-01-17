---
title:                "Lähettämässä http-pyyntöä"
html_title:           "Haskell: Lähettämässä http-pyyntöä"
simple_title:         "Lähettämässä http-pyyntöä"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
HTTP-pyynnön lähettäminen on vain tapa lähettää viesti web-palvelimelle. Ohjelmoijat tekevät tätä kommunikoidakseen eri verkkopalveluiden kanssa ja hakeakseen tietoa.

## Miten:
```Haskell
import Network.HTTP
import Network.HTTP.Headers

makeHTTPRequest :: String -> IO ()
makeHTTPRequest url = do
  resp <- simpleHTTP (getRequest url)
  case resp of
    Left _ -> putStrLn "An error occurred"
    Right r -> do
      let status = rspCode r
      let body = rspBody r
      putStrLn "Status code:"
      print status
      putStrLn "Body:"
      putStrLn body
      
makeHTTPRequest "https://www.example.com"
```

Ulostulo:
```
Status code:
2XX
Body:
<html> <body> Hello, world! </body> </html>
```

## Syvädykkäys:
HTTP-protokollan alkuperäinen tarkoitus oli luoda yhteys web-palvelimen ja asiakkaan välille. Nykypäivänä on olemassa muita vaihtoehtoja, kuten WebSockets, mutta HTTP-pyynnöt ovat edelleen tärkeitä esimerkiksi RESTful-rajapintojen toteuttamisessa. HTTP-pyynnön toteuttaminen Haskellilla tapahtuu käyttämällä Network.HTTP-moduulia ja sen tarjoamia toimintoja.

## Katso myös:
- [HaskellWiki - Network.HTTP](https://wiki.haskell.org/Network.HTTP)
- [Hackage - Network.HTTP](https://hackage.haskell.org/package/HTTP)