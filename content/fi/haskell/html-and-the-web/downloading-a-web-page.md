---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/haskell/downloading-a-web-page.md
date:                  2024-01-20T17:44:20.095762-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? - Mikä ja Miksi?
Ladataan nettisivu, eli noudetaan webbisivun sisältö ohjelmoitavasti. Ohjelmoijat tekevät tämän datan keräämiseen, testaukseen tai palvelujen automatisointiin.

## How to: - Kuinka tehdään:
Haskellilla webbisivujen lataaminen onnistuu kirjastoilla kuten `http-conduit`. Asentakaa ensin tarvittavat kirjastot:

```haskell
-- web-sivun lataamiseen
import Network.HTTP.Simple

-- esimerkkikoodi web-sivun lataamiseksi
main :: IO ()
main = do
    response <- httpLBS "http://example.com"
    let statusCode = getResponseStatusCode response
    if statusCode == 200
        then putStr "Sivun lataus onnistui!\n"
        else putStr "Jokin meni pieleen.\n"
    print $ getResponseBody response
```

Suoritetaan ja saadaan tulosteeksi sivun sisältö, tai virheviesti jos lataus epäonnistuu.

## Deep Dive - Syväsukellus:
Web-sivujen lataus Haskellissa on kehittynyt ajan myötä. Alkuaikoina käytettiin peruskirjastoja kuten `Network.HTTP`, mutta moderneissa sovelluksissa siirryttiin korkeamman tason `http-conduit`-kaltaisiin ratkaisuihin, joissa tietoturva ja helppokäyttöisyys ovat parempia. Vaihtoehtoja ovat muun muassa `http-client` ja `wreq`. Ne tarjoavat erilaista käytettävyyttä ja suorituskykyä.

Toimiakseen, nämä kirjastot hyödyntävät Haskellin laiskan evaluaation ominaisuutta. Se mahdollistaa tehokkaan datan käsittelyn ilman tarvetta ladata koko sivun sisältöä muistiin. Yksi huomionarvoinen seikka on, että koneen verkkoyhteyden asetukset voivat vaikuttaa latausprosessiin.

## See Also - Katso Myös:
- Haskell `http-conduit` -kirjaston dokumentaatio: [http-conduit on Stackage](https://www.stackage.org/package/http-conduit)
- `Network.HTTP.Simple` API-esimerkit: [Network.HTTP.Simple](https://hackage.haskell.org/package/http-conduit-2.3.8/docs/Network-HTTP-Simple.html)
- Haskellin virallinen oppaan verkossa: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- HTTP-protokollan yleiskatsaus: [HTTP - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP)
