---
title:                "Fish Shell: Lähettäminen http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnilla"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

### Miksi

Miksi joku haluaisi lähettää HTTP-pyynnön perusautentikoinnilla? HTTP-pyynnöt perusautentikoinnilla ovat hyödyllisiä, kun haluat suojata luottamuksellista tietoa, kuten salasanoja, käyttäjätunnuksia tai muita arkaluonteisia tietoja, lähettämällä näitä tietoja suojatun yhteyden kautta.

### Miten

Koodiesimerkkien ja näytetulosteiden avulla esittelemme, miten lähetät HTTP-pyynnön perusautentikoinnin avulla käyttämällä Fish Shell -ohjelmaa.

````Fish Shell
# Määritä käyttäjätunnus ja salasana muuttujiin
set username "käyttäjätunnus"
set password "salasana"

# Lähetä HTTP-pyyntö osoitteeseen
curl -u $username:$password https://esimerkkisivu.fi/api
````

Tässä koodiesimerkissä käytetään "curl" -työkalua, joka on käytettävissä useimmissa käyttöjärjestelmissä. Käyttäjätunnus ja salasana tallennetaan muuttujiin ja ne syötetään "-u" parametrin avulla, joka määrittää käyttäjätunnuksen ja salasanan HTTP-pyynnön otsakkeessa. Tämän jälkeen "curl" lähettää pyynnön ja saamme vastauksena olevan datan näytteen.

### Syvemmälle

HTTP-pyynnön lähettäminen perusautentikoinnilla vaatii tietynlaisen muotoilun pyynnön otsakkeessa. Yleensä käyttäjätunnus ja salasana erotetaan kaksoispisteellä ja koodataan Base64-muotoon. Tämä salaavuus ei kuitenkaan ole optimaalinen, joten on tärkeää käyttää suojattua yhteyttä ("https") varmistaaksesi, että tiedot eivät ole helposti haavoittuvia.

### Katso myös

- Fish Shell: https://fishshell.com/
- cURL: https://curl.se/
- HTTP Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme