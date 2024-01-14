---
title:                "Arduino: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi ohjelmoida HTML:n parsimista Arduinolla? HTML on verkkosivustojen yleisin merkkauskieli ja sen avulla pystytään luomaan monipuolisia ja interaktiivisia sivustoja. Arduino on alusta, joka antaa mahdollisuuden muokata erilaisia laitteita ja luoda erilaisia projekteja, joten mahdollisuus käyttää HTML:ää Arduinon kanssa voi avata ovia monille mielenkiintoisille projekteille.

## Miten tehdä

HTML:n parsiminen Arduinolla voi vaikuttaa monimutkaiselta, mutta se on mahdollista käyttämällä "HTTPClient" -kirjastoa, joka on saatavilla Arduinon yhteisön tukemana kirjastojaosta. Seuraavassa esimerkissä käytetään "HTTPClient" -kirjaston "getString" -toimintoa, joka hakee verkkosivun HTML-koodin ja tallentaa sen merkkijonona:

```Arduino
#include <HTTPClient.h> // Liitetään kirjasto

HTTPClient http; // Luodaan HTTPClient-olio
String htmlCode; // Määritellään muuttuja HTML-koodin tallentamista varten

void setup() {
  Serial.begin(9600); // Alustetaan sarjaliikenneliitäntä
  // Yhdistetään verkkoon
  WiFi.begin("SSID", "Password");
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Yhdistetään...");
  }
  Serial.println("Yhdistetty!");
}

void loop() {
  // Määritetään HTTP-osoite, josta halutaan hakea HTML-koodia
  http.begin("https://www.example.com");
  
  // Lähetetään pyyntö ja tallennetaan vastaus "htmlCode"-muuttujaan
  htmlCode = http.getString();
  
  // Tulostetaan HTML-koodi sarjaporttiin
  Serial.println(htmlCode);
  
  delay(5000); // Odottaa 5 sekuntia ennen seuraavaa päivitystä
}
```

### Esimerkkitulos

Yllä olevan koodin suorittamisen jälkeen sarjaporttiin tulisi ilmestyä kyseisen verkkosivun HTML-koodi merkkijonona.

## Syvällinen sukellus

HTML:n parsiminen ei rajoitu vain yksittäisen verkkosivun hakemiseen, vaan sen avulla voidaan myös hakea ainoastaan tiettyjä osia verkkosivusta. Tämä on hyödyllistä esimerkiksi, jos halutaan kerätä tietoa sivustolta ja käyttää sitä johonkin muuhun tarkoitukseen.

"HTTPClient":n "getString" -toiminto palauttaa kaiken HTML-koodin, mutta "HTTPClient":n avulla on myös mahdollista hakea ja parsia tiettyjä elementtejä sivulta käyttämällä "find" -toimintoa. Esimerkiksi seuraavan koodin avulla voitaisiin hakea ja tallentaa verkkosivun otsikko HTML-koodista:

```Arduino
// Haetaan ensin sivun koko HTML-koodi ja tallennetaan se muuttujaan
String htmlCode = http.getString();

// Käytetään "find"-toimintoa hakemaan tiettyä elementtiä sivulta
String title = htmlCode.substring(htmlCode.indexOf("<title>") + 7, htmlCode.indexOf("</title>"));

// Tulostetaan otsikko sarjaporttiin
Serial.println("Sivun otsikko on: " + title);
```

### Lisäresursseja

* [HTTPClient-kirjaston esimerkkejä (englanniksi)](https://github.com/arduino-libraries/HTTPClient/tree/master/examples)
* [HTTPClient-kirjaston dokumentaatio (englanniksi)](https://github.com/jar