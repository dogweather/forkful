---
title:                "Haskell: http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi 
 Yksi yleinen haaste ohjelmoinnissa on lähettää pyyntöjä ulkoisille tietolähteille, esimerkiksi verkkosivustoille tai palvelimille. HTTP-pyyntöjen avulla voimme hakea ja lähettää tietoa näiltä lähteiltä. Jotta voimme varmistaa, että pyyntöjämme käsitellään turvallisesti ja luotettavasti, meidän on usein myös todennettava itsesi käyttämällä basic authentication (perustodentaminen). Tämä blogikirjoitus auttaa meitä ymmärtämään, kuinka toteuttaa tämä Haskellilla ja miksi se voi olla hyödyllistä.

## Kuinka 
 Aloitetaan luomalla perus HTTP-pyyntö Haskellilla ja lisäämällä siihen perustodentaminen. Tämä voidaan tehdä helposti käyttämällä `http-conduit` -pakettia.  Käydään läpi vaiheet yksi kerrallaan: 

```Haskell
import Network.HTTP.Simple 
import qualified Network.HTTP.Client as Client 
  
main = do 
    -- Luodaan yhteys haluttuun sivustoon 
    request <- parseRequest "https://example.com/" 
    
    -- Lisätään basic authentication 
    let auth = "username:password" 
        requestWithAuth = setRequestBasicAuth (C.pack auth) request 
        -- Korvataan "username" ja "password" omilla tiedoillasi 
    -- Lähetetään pyyntö ja tallennetaan vastaus 
    response <- httpLBS requestWithAuth 
    
    -- Tulostetaan vastauksen sisältö 
    print $ getResponseBody response
```

Kun ajamme tämän koodin, saamme vastauksen sisällön tulostettuna konsoliin.

```
"<html> <h1>Tervetuloa!</h1> </html>"
```

Nyt olemme onnistuneesti lähettäneet HTTP-pyynnön osoitteeseen `https://example.com/` ja saaneet vastauksen.

## Syvempi sukellus 
Jos haluamme tarkemmin määrittää pyyntömme, voimme käyttää `Request` -tyyppiä ja asettaa haluamiamme parametreja. Esimerkiksi voimme määrittää `Request` -tyypin `setRequestPort` -funktiolla, joka antaa meille mahdollisuuden valita haluamamme portin.

```Haskell
import Network.HTTP.Simple 
import qualified Network.HTTP.Client as Client 
  
main = do 
    request <- parseRequest "https://example.com/" 
    let auth = "username:password" 
        requestWithAuth = setRequestBasicAuth (C.pack auth) request 
    -- Määritetään portti 
    let requestWithPort = setRequestPort 5000 requestWithAuth 
    -- Lähetetään pyyntö 
    response <- httpLBS requestWithPort 
    
    print $ getResponseBody response
```

Voimme myös lisätä muita tärkeitä parametreja, kuten otsikkoja, kuten `setRequestHeader` -funktiolla.

```
main = do 
    request <- parseRequest "https://example.com/" 
    let auth = "username:password"
        requestWithAuth = setRequestBasicAuth (C.pack auth) request 
    -- Lisätään otsikko 
    let requestWithHeader = setRequestHeader "Content-Type" ["application/json"] requestWithAuth
    response <- httpLBS requestWithHeader 
    print $ getResponseBody response
```

## Katso myös 
- [Haskell HTTP-pyyntöjen tekeminen `http-conduit` -paketilla](https://www.yesodweb.com/book/http-conduit)
- [Haskell HTTP-paketin dokumentaatio](https://hackage.haskell.org/package/http-client)
- [HTTP-pyyntöjen toteuttaminen Haskellillä Nettipäiväkirjassa](https://www.nettipaivakirja.com/haskell-http-pyyntojen-toteuttaminen/)