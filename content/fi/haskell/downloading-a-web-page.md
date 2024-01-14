---
title:                "Haskell: Web-sivun lataaminen"
simple_title:         "Web-sivun lataaminen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Miksi: Miksi ladata verkkosivu?

Verkkosivujen lataaminen on tärkeä osa Haskell-ohjelmointia, sillä se mahdollistaa tietojen hakemisen ja hyödyntämisen verkosta. Monet sovellukset ja työkalut, kuten nettiselaimet ja hakukoneet, käyttävät verkkosivujen lataamista tietojen välittämiseen käyttäjille. Lisäksi ladataan myös esimerkiksi uutissivuja ja sääasemia.

# Kuinka: Koodiesimerkkejä ja tulostusta

Verkkosivujen lataaminen on mahdollista Haskellilla erilaisten kirjastojen, kuten "http-client" tai "wget" avulla. Alla olevassa koodiesimerkissä käytetään "http-client"-kirjastoa ja ladataan yksinkertaisella GET-pyynnöllä sivusto www.example.com:

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS

-- Luodaan http-client -kirjaston avulla HTTP-client ja asetetaan sille käytettäväksi TLS.
main :: IO ()
manager <- newTlsManager
let request = "GET" "http://www.example.com"
response <- httpLbs request manager
-- Tulostetaan vastauksen statuskoodi ja koko sivun sisältö.
putStrLn $ "Response status: " ++ show (responseStatus response)
putStrLn $ "Response body: " ++ responseBody response
```

Tulostettuun tekstiin voi lisätä myös muotoilua, kuten kaunisulkoasuisen HTML-sisällön tai käsitellä dataa edelleen esimerkiksi "html-conduit"-kirjaston avulla.

# Syvemmälle: Tarkempaa tietoa verkkosivujen lataamisesta

Verkkosivujen lataaminen on monimutkainen prosessi, joka sisältää useita vaiheita. Ensin luodaan yhteys haluttuun verkkosivuun ja lähetetään pyyntö. Tämän jälkeen vastaus käsitellään ja tarvittavat tiedot haetaan sivun sisällöstä. Lopuksi yhteys suljetaan.

Verkkosivujen lataamisessa tulee myös ottaa huomioon mahdolliset virheet, kuten epäkelvot linkit tai sivulle pääsy vaatii käyttäjän tunnistautumisen. Tällaisissa tilanteissa on tärkeää käsitellä virheilmoitukset ja osata ohjata ohjelma oikeisiin toimenpiteisiin.

Näiden lisäksi on tärkeää käydä läpi verkkosivujen rakennetta ja ymmärtää HTML-kielen perusteet, sillä se on yleisin tapa esittäyä sivustojen sisältöjä ja tietoja.

# Katso myös

- [HTTP-client -kirjaston dokumentaatio](https://hackage.haskell.org/package/http-client)
- [html-conduit -kirjaston dokumentaatio](https://hackage.haskell.org/package/http-conduit)
- [Haskellin perusteet](https://wiki.haskell.org/Learn_Haskell)