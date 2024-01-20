---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on prosessi, jossa sovelluksemme pyytää tietoja tai tekee toimintoja web-palvelimelle. Ohjelmoijat tekevät sen vuorovaikutuksessa verkkoresurssien kanssa, kuten API:iden tai webbisivustojen kanssa.

## Miten se tehdään:

Haskellin palveluntarjoajia varten voit käyttää `http-client` -pakettia. Tässä on esimerkki yksinkertaisesta GET-pyynnöstä:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    print $ responseBody response
```
Kun koodi suoritetaan, se saa HTTP-pyynnön httpbin.org/getista ja tulostaa sekä paluukoodin että vastauksen sisällön.

## Syvempi sukellus:

Haskellissa HTTP-pyynnön lähettämisen mahdollistaa `http-client`-kirjaston takana oleva alhainen verkko-IO. Se on suunniteltu olemisen yhtenäinen ja joustava, jonka avulla voit mukauttaa pyyntösi tarpeidesi mukaan.

Historia tekee mielenkiintoista lukemista: Elonatan Roodo teki ensimmäisen Haskell HTTP-kirjaston julkaisun vuonna 2004. Sen jälkeen `http-client` on ottanut johtoaseman, tarjoten vahvan ja joustavan HTTP-clientin haskell-yhteisölle.

Vaihtoehtona on myös `http-conduit`-paketti, joka on rakennettu päälle `http-client` ja tarjoaa korkeamman tason abstraktiot verkkoyhteyksille.

Rakenteissa on useita vaiheita, mukaan lukien URL:n jäsentäminen, oikean HTTP-menetelmän valinta ja tietyntyyppisten pyyntöjen eriyttäminen.

## Katso myös:

Lisätietoja Haskellin HTTP-asiakaskirjastoista löydät seuraavista lähteistä:

1. http-client: https://hackage.haskell.org/package/http-client
2. http-conduit: https://hackage.haskell.org/package/http-conduit
3. Network.HTTP.Client: https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Client.html
4. Haskell Wiki - HTTP: https://wiki.haskell.org/HTTP