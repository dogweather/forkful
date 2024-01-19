---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Web-sivun lataaminen tarkoittaa sivun datan siirtämistä internetistä omaan koneeseen. Ohjelmoijat tekevät tämän tiedon keräämisen, analysoinnin tai offline-käyttöön liittyvien tehtävien vuoksi.

## Näin se tehdään:

Haskellilla voit ladata verkkosivun käyttämällä kirjastoa nimeltä `http-conduit`. Katsotaanpa esimerkkiä:

```haskell
import Network.HTTP.Simple

main :: IO ()
main = do
    response <- httpBS "http://example.com"
    putStrLn $ B.unpack $ getResponseBody response
```

Tämä ohjelma lataa etusivun "example.com"-verkkosivulta ja tulostaa vastauksen.

## Sukellus syvälle:

Historiallisessa kontekstissa, web-sivujen lataaminen on ollut osa ohjelmointia Internetin alusta alkaen sen tarjoaman tiedon vuoksi. Haskellin `http-conduit`-kirjasto on yksi tapa toteuttaa se, mutta on olemassa myös muiden ohjelmointikielten kirjastoja, kuten Pythonin `requests` tai JavaScriptin `axios`.

Web-sivun lataaminen voi tuntua yksinkertaiselta, mutta sen toteutuksessa on paljon yksityiskohtia. HTTP-pyynnöt, otsikot, vastauksen käsittely, virheiden käsittely, ja paljon muuta. 

Ainoa sääntö on, että ongelmien välttämiseksi sinun on aina noudatettava verkkosivuston käyttöehtoja ja robot.txt-tiedostoa lataessasi web-sivuja.

## Katso myös:

Seuraavat linkit tarjoavat hyödyllistä tietoa ja syvällisempää tietoa aiheesta:

1. [How to Download a Web Page in Haskell](https://www.stackbuilders.com/tutorials/haskell/web-scraping-wreq/)
2. [http-conduit GitHub](https://github.com/snoyberg/http-client)
3. [Scraping the Web in Haskell](https://aikikode.github.io/posts/2020-04-03-web-scraping-with-haskell/)
4. [Robot Exclusion Standard](https://www.robotstxt.org/orig.html)