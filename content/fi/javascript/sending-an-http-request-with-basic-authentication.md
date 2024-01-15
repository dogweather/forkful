---
title:                "Lähettäminen http-pyynnöstä perusautentikoinnilla"
html_title:           "Javascript: Lähettäminen http-pyynnöstä perusautentikoinnilla"
simple_title:         "Lähettäminen http-pyynnöstä perusautentikoinnilla"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Javascriptia käytetään usein verkkosivujen kehittämisessä ja yhtenä tärkeänä osana on kommunikaatio eri palvelimien välillä. HTTP-pyyntöjen lähettäminen on yksi tapa lähettää ja vastaanottaa tietoja palvelimelta. Basic Authenticationin käyttö yhdessä HTTP-pyyntöjen kanssa mahdollistaa turvallisen tiedonvälityksen palvelimien välillä.

## Kuinka

```Javascript
// Luodaan uusi XMLHttpRequest-olio
var xhttp = new XMLHttpRequest();

// Määritellään HTTP-pyynnön tyyppi ja osoite
xhttp.open("GET", "https://esimerkkisivu.com/api/tiedot", true);

// Lisätään käyttäjätunnus ja salasana requestin headeriin
var encodedCredentials = btoa("kayttajatunnus:salasana"); // Muutetaan käyttäjätunnus ja salasana Base64-muotoon
xhttp.setRequestHeader("Authorization", "Basic " + encodedCredentials);

// Lähetetään pyyntö ja käsitellään vastaus
xhttp.send();
xhttp.onreadystatechange = function() {
  if (this.readyState == 4 && this.status == 200) {
    // Vastaus saapui onnistuneesti
    console.log(this.responseText);
  } else {
    // Jokin meni vikaan
    console.log("Virhe! Tarkista käyttäjätunnus ja salasana.");
  }
};
```

```Javascript
// Palvelinpään koodi (esim. Node.js)
var http = require('http');
var username = 'kayttajatunnus';
var password = 'salasana';

http.createServer(function (req, res) {

    // Tarkistetaan pyynnön authentikaatio headerista
    var auth = req.headers['authorization'];
    if (!auth || auth.indexOf('Basic ') !== 0) {
        // Pyyntö ei sisältänyt authentikaatiota
        res.statusCode = 401;
        res.setHeader('WWW-Authenticate', 'Basic realm="Sisäänkirjautuminen vaaditaan"');
        res.end('Käyttäjätunnus ja salasana vaaditaan.');
        return;
    }

    // Tarkistetaan käyttäjätunnus ja salasana
    var credentials = new Buffer(auth.split(' ')[1], 'base64').toString();
    var user = credentials.split(':')[0];
    var pass = credentials.split(':')[1];
    if (user !== username || pass !== password) {
        // Väärä käyttäjätunnus tai salasana
        res.statusCode = 401;
        res.setHeader('WWW-Authenticate', 'Basic realm="Väärä käyttäjätunnus tai salasana"');
        res.end('Käyttäjätunnus tai salasana on väärä.');
        return;
    }

    // Palautetaan vastaus
    res.end('Oikeat tiedot, pääset eteenpäin!');
}).listen(3000);
```

Esimerkkikoodissa luodaan uusi XMLHttpRequest-olio ja määritellään sen avulla GET-pyyntö osoitteeseen https://esimerkkisivu.com/api/tiedot. Sen jälkeen lisätään käyttäjätunnus ja salasanapyynnön headeriin Base64-muodossa ja lähetetään pyyntö. Palvelinpuolella pyynnön authentikaatio tarkistetaan ja jos tiedot ovat oikein, palautetaan vastaus onnistuneen authentikaation merkiksi.

## Syväsukellus

HTTP-pyyntöjen englanninkielinen nimi on "HTTP request" ja se on tapa lähettää ja vastaanottaa tietoja palvelimien välillä. Pyyntö koostuu useista osista, kuten metodista (GET, POST jne.), osoitteesta ja mahdollisista parametreista. Basic Authentication mahdollistaa käyttäjän tunnistamisen pyynnön header:ssä. Salasana kuitenkin lähetetään Base64-muodossa, mikä ei ole täysin tur