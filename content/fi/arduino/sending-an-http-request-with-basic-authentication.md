---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTTP-pyyntö perusautentikoinnilla on web-sovellusten tapa siirtää tietoja verkossa turvallisesti. Ohjelmoijat käyttävät sitä, jotta tiedot eivät eksyisi ulkopuolisille.

## Näin se tehdään:

HTTP-pyynnön lähettäminen Arduino-levyltä vaatii Ethernet-kirjaston. 

Aloitamme määrittämällä verkon asetukset:

```Arduino
#include <Ethernet.h>

byte mac[] = {  
  0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED
};

IPAddress server(74,125,232,128); // Google

EthernetClient client;
```
Seuraavaksi luomme `setup()`-funktion, jossa aloitamme Ethernet-yhteyden ja tulostamme Ethernet-asetukset:

```Arduino
void setup()
{
  Ethernet.begin(mac);
  Serial.begin(9600);

  while (!Serial) {
    ;
  }

  delay(1000);
  
  Serial.println(Ethernet.localIP());
}

```
Lopuksi luomme `loop()`-funktion, jossa lähetämme HTTP-pyynnön:

```Arduino
void loop()
{
  if (client.connect(server, 80)) {
    client.println("GET /search?q=arduino HTTP/1.1");
    client.println("Host: www.google.com");
    client.println("Authorization: Basic ");
    client.println("Connection: close");
    client.println();
  } 
  else {
    Serial.println("connection failed");
  }

  delay(10000);
}
```
## Syvällisempi sukellus

HTTP-pyynnön perusautentikointi on ollut järjestelmien sisäänkirjautumisen selkäranka jo vuosikymmenten ajan. Se on vanha, mutta edelleen suosittu tapa siirtää tietoja verkossa auktorisoidun käyttäjän toimesta. Vaikka monitahoisemmat autentikointiin perustuvat järjestelmät, kuten OAuth, ovat näinä päivänä enemmän normi, perusautentikointi pysyy edelleen yksinkertaisena ja tehokkaana vaihtoehtona.

HTTP-pyynnöt perusautentikoinneilla on helppo toteuttaa Arduino-laitteissa kämin Ethernet-kirjastoa. Kun se on määritelty, voit ohjata verkkoliikennettä helposti. Vaikka on tärkeää huomata, että perusautentikointi lähetetään Base64-koodauksessa eikä se ole salattu. Siksi se ei ole ihanteellinen ratkaisu herkkien tietojen käsittelyyn.

## Katso myös:

Jos haluat lukea lisää HTTP-pyynnöstä perusautentikointia ja muita tapoja käsitellä verkkoliikennettä Arduinolla, voit tutustua seuraaviin resursseihin:

1. Arduino Ethernet Library: https://www.arduino.cc/en/Reference/Ethernet
2. HTTP Basic Authentication: https://tools.ietf.org/html/rfc7617
3. Miten käyttää Arduinon Ethernet Shield: http://www.circuitbasics.com/how-to-set-up-the-dht11-humidity-sensor-on-an-arduino/
4. Miten salata HTTP-pyynnöt: https://www.arduino.cc/en/Tutorial/WebClientRepeating
5. Arduino Ethernet Shieldin ohjelmointi: https://www.instructables.com/Programming-the-Arduino-Ethernet-Shield