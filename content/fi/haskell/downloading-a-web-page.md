---
title:                "Verkkosivun lataaminen"
html_title:           "Haskell: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Mikä ja Miksi?

Ladataan sivu tarkoittaa käytännössä sivun sisällön kopioimista internetistä tietokoneellesi. Tätä tehdään yleensä sen takia, että haluat käyttää sivun sisältöä jossakin muussa sovelluksessa tai analysoit sivun tietoja.

# Kuinka:

```Haskell
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L

main :: IO ()
main = do
  response <- httpLBS "http://example.com"
  L.putStrLn (getResponseBody response)
```

Tämä koodi käyttää `httpLBS` funktiota ladataakseen sivun ja tulostaa sen sisällön konsoliin. Tämä vaatii `Network.HTTP.Simple` kirjaston tuomisen. Voit muuttaa sivun osoitteen, jolloin koodi lataa haluamasi sivun.

# Syväsukellus:

Sivun lataaminen on yleinen tehtävä monissa ohjelmointitilanteissa. Esimerkiksi web-scraperit käyttävät tätä toimintoa hakeakseen tietoa sivuilta automaattisesti. On myös muita tapoja ladata sivuja, kuten käyttämällä HTTP-pyynnön tuloksena olevaa vastauskoodia. Haskellissa on myös muita kirjastoja, jotka tarjoavat lisäominaisuuksia sivujen lataamiseen, kuten `Http-Client` ja `Curl`.

# Katso myös:

- [Haskellin virallinen dokumentaatio sivujen lataamisesta](https://hackage.haskell.org/package/http-client)
- [HTTP-pyynnön vastauskoodit](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [Mikä on web-scraper?](https://en.wikipedia.org/wiki/Web_scraping)