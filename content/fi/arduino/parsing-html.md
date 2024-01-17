---
title:                "HTML:n jäsentäminen"
html_title:           "Arduino: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

HTML-parsiminen tarkoittaa tekstin analysoimista ja siitä tiettyjen tietojen poimimista. Se on tärkeä työkalu ohjelmoijille, jotka haluavat käsitellä ja käyttää web-sivuilla olevaa tietoa omassa koodissaan.

## Miten:

Arduino-tehtävässä HTML-parsiminen voidaan suorittaa esimerkiksi käyttämällä ESP8266WiFi-kirjastoa. Tässä esimerkissä käytämme HTTP-pyyntöä ja HTML-parsimista saadaksemme tietyn otsikon web-sivulta ja tulostamme sen sarjamonitoriin:

```Arduino
#include <ESP8266WiFi.h>

void setup() {
  Serial.begin(115200); // alustetaan sarjamonitori
  WiFi.begin("ssid", "password"); // yhdistetään WiFi-verkkoon
  while (WiFi.status() != WL_CONNECTED) { // odotetaan yhdistymistä
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.println("WiFi-yhteys muodostettu!");
  Serial.print("IP-osoite: ");
  Serial.println(WiFi.localIP()); // tulostetaan IP-osoite
  Serial.println("Hakee sivua...");
  String sivu = httpGet("www.esimerkkisivu.com"); // haetaan web-sivu
  String otsikko = parseHTML(sivu); // parsitaan otsikko
  Serial.print("Web-sivun otsikko: ");
  Serial.println(otsikko); // tulostetaan otsikko
}

String httpGet(String osoite) {
  WiFiClient client; // luodaan WiFiClient-olio
  const int httpPort = 80;
  if (!client.connect(osoite, httpPort)) { // muodostetaan yhteys
    Serial.println("Virhe yhteyden muodostamisessa.");
    return "";
  }
  client.print(String("GET / HTTP/1.1\r\n") + // lähetetään HTTP-pyyntö
               String("Host: ") + osoite + "\r\n" +
               String("User-Agent: ESP8266/1.0\r\n") +
               String("Connection: close\r\n\r\n") );
  delay(10);
  while (client.connected()) { // odotetaan vastausta
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break;
    }
  }
  String response = client.readString(); // ladataan vastaus
  client.stop(); // suljetaan yhteys
  return response;
}

String parseHTML(String sivu) {
  int alku = sivu.indexOf("<title>") + 7; // etsitään alkumerkki
  int loppu = sivu.indexOf("</title>", alku); // etsitään loppumerkki
  String otsikko = sivu.substring(alku, loppu); // poimitaan osa
  return otsikko;
}

void loop() {
  // ei tehdä mitään
}
```

Kun koodi suoritetaan, sarjamonitoriin tulostuu seuraavaa:

```
WiFi-yhteys muodostettu!
IP-osoite: 192.168.1.32
Hakee sivua...
Web-sivun otsikko: Esimerkkisivu – Tervetuloa!
```

## Syväsukellus:

HTML-parsinta on osa web-scrapingia, joka on tekniikka, jolla dataa kerätään web-sivuilta automatisoidusti. Sitä voidaan käyttää esimerkiksi hintojen vertailuun, urheilutulosten keräämiseen tai muihin vastaaviin tarkoituksiin. Myös muita tapoja parsia HTML:ää on olemassa, kuten käyttämällä XPath-kyselyitä.

## Katso myös:

- https://www.w3schools.com/xml/xpath_intro.asp
- https://www.arduino.cc/reference/tr/language/functions/communication/httpget.aspx
- https://www.arduino.cc/reference/tr/libraries/wifiniclient