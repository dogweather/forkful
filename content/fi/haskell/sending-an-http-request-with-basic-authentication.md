---
title:                "Perusautentikoinnin käyttö http-pyynnössä"
html_title:           "Haskell: Perusautentikoinnin käyttö http-pyynnössä"
simple_title:         "Perusautentikoinnin käyttö http-pyynnössä"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

HTTP-pyyntöjen lähettäminen perusautentikoinnilla tarkoittaa yksinkertaisuudessaan HTTP-käytännön käyttämistä salasanan lähettämiseen pyynnölle. Tämä on tärkeää esimerkiksi, kun käytetään API-rajapintoja, jotta voidaan osoittaa pääsyoikeus palveluntarjoajan tarjoamiin tietoihin.

# Kuinka:

Esimerkiksi, jos haluat lähettää GET-pyynnön, joka sisältää käyttäjätunnuksen ja salasanan, voit tehdä sen seuraavasti:

```Haskell
import Network.HTTP
import Network.HTTP.Headers

-- Luodaan HTTP-pyynnön otsikko, jossa käyttäjätunnus ja salasana base64-koodataan.
authHeader = mkHeader HdrAuthorization ("Basic " ++ (encodeString "käyttäjätunnus:salasana"))

-- Luodaan GET-pyyntö ja lisätään luotu otsikko.
request = getRequest "http://esimerkki.com/api/data"
withAuthority request authHeader

-- Lähetetään pyyntö ja tulostetaan vastauksen koodi ja sisältö.
response = simpleHTTP request
print $ rspCode response
print $ rspBody response
```

Tämä koodi lähettää GET-pyynnön osoitteeseen "http://esimerkki.com/api/data", jossa käyttäjätunnus on "käyttäjätunnus" ja salasana on "salasana". Huomaa, että tässä käytetään vain Network.HTTP-moduulin toimintoja, joten se toimii myös Windowsilla.

# Syväsukellus

Perusautentikointi on yksi vanhimmista ja yksinkertaisimmista tavoista suojata HTTP-pyyntöjä ja -vastauksia. Siinä käyttäjätunnus ja salasana base64-koodataan ja lähetetään Pohja-autentikointi HTTP-pyyntöjen kautta. Tämä autentikointimuoto ei kuitenkaan ole erityisen turvallinen, sillä käyttäjätunnuksen ja salasanan base64-koodi voidaan helposti purkaa takaisin selkokieliseen muotoon. Siksi sen käyttöä ei suositella, jos tiedonsuoja on ensisijaisen tärkeää.

On myös olemassa muita tapoja lähettää pyyntöjä perusautentikoinnilla, kuten käyttäjätunnuksen ja salasanan lisääminen URL-osoitteeseen tai käyttämällä pyyntöä palvelun tarjoaman kirjautumissivun kautta.

HTTP-pyyntöjen lähettämiseen perusautentikoinnilla on olemassa useita tapoja Haskellissa, kuten myös muissa ohjelmointikielissä. Ne vaihtelevat käyttöliittymiltään ja ominaisuuksiltaan, mutta perusperiaatteet pysyvät samoina.

# Katso myös:

- Network.HTTP-moduulin dokumentaatio: https://hackage.haskell.org/package/HTTP/docs/Network-HTTP.html
- Perusautentikoinnin käyttöoikeus HTTP:llä: https://developer.mozilla.org/fi/docs/Web/HTTP/Authentication