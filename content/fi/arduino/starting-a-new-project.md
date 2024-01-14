---
title:                "Arduino: Uuden projektin aloittaminen"
programming_language: "Arduino"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi
Arduino-ohjelmoinnin aloittaminen voi olla innostava ja palkitseva harrastus, jossa voit tuoda ideoitasi ja luovuuttasi konkreettiseen muotoon. Se haastaa sinua oppimaan uusia taitoja ja kehittämään projektisi edistymisen myötä.

## Miten aloittaa
Aloittaaksesi uuden projektin Arduino-ohjelmoinnissa, tarvitset ensin muutaman peruskomponentin, kuten Arduino-mikrokontrollerin, USB-kaapelin ja kytkentäkaavion. Sitten voit ladata Arduinon ohjelmointiympäristön ja aloittaa koodaamisen. Tässä on yksinkertainen esimerkkikoodi ja sen tulostus:

```Arduino
void setup() {
  Serial.begin(9600); //Aseta sarjaportin nopeus
}

void loop() {
  Serial.println("Hei maailma!"); //Tulosta "Hei maailma!" sarjaportille
  delay(1000); //Odota 1 sekunti
}
```

Tulostus:

```
Hei maailma!
Hei maailma!
Hei maailma!
...
```

Koodissa voidaan käyttää myös muita komponentteja, kuten LED-valoja, antureita tai moottoreita, ja näin luoda yksinkertaisia projekteja, kuten valojen vilkkuminen tai etäisyysmittari.

## Syvemmälle
Kun olet saanut hyvän otteen Arduinon ohjelmoinnista, voit lähteä syventymään monipuolisempiin projekteihin. Voit esimerkiksi hyödyntää ulkoisia kirjastoja ja toimintoja, kuten WiFi-yhteyksiä, Bluetooth-yhteyksiä ja GPS-paikannusta. Voit myös laajentaa projektiasi siten, että se kommunikoi tietokoneiden kanssa tai käyttää erilaisia visualisointityökaluja.

### Esimerkki: Lämpötila- ja kosteusmittari
Alla oleva esimerkkikoodi mittaa ympäristön lämpötilaa ja kosteutta DHT11-anturin avulla ja lähettää tiedot tietokoneelle sarjaportin kautta. Voit käyttää tietokoneen johonkin visualisointiohjelmaan, kuten Exceliin, luomaan kaavioita seurattavista tiedoista.

```Arduino
#include <DHT.h> //Lisää DHT-kirjasto
#define DHTPIN 2 //Määritä DHT11-anturin näppäimistö liitäntä
#define DHTTYPE DHT11 //Määritä käytetty anturityyppi
DHT dht(DHTPIN, DHTTYPE); //Luo DHT objekti

void setup() {
  Serial.begin(9600); //Aseta sarjaportin nopeus
  dht.begin() //Alusta DHT11-anturi
}

void loop() {
  float temperature = dht.readTemperature(); //Lue ja tallenna lämpötila muuttujaan
  float humidity = dht.readHumidity(); //Lue ja tallenna kosteus muuttujaan
  Serial.print("Lämpötila: "); //Tulosta teksti
  Serial.print(temperature); //Tulosta muuttujan arvo
  Serial.println(" C"); //Tulosta loppuosa tiedosta
  Serial.print("Kosteus: ");
  Serial.print(humidity);
  Serial.println(" %");
  delay(1000); //Odota 1 sekunti
}
```

Tulostus:

```
Lämpötila: 20.00 C
Kosteus: 50.00 %
```

## Katso myös
- [Arduino:n virallinen sivusto](https://www.arduino.cc/)
- [Arduino-projekteja aloittelijoille](https://create.arduino.cc/projecthub)
- [Arduino-kirjastojen luettelo](https