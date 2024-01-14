---
title:    "Arduino: Vastaisen päivämäärän hankkiminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Mikään Arduino-projekti ei ole täydellinen ilman tarkkaa ja ajankohtaista päivämäärää. Olitpa rakentamassa kelloa, ajastettua valojärjestelmää tai muuta laitetta, jossa tarvitset päivämäärän, Arduino tarjoaa helpon tavan hankkia ja näyttää sen.

## Kuinka

Ennen kuin voimme hankkia nykyisen päivämäärän, meidän on ensin määriteltävä tarvittavat muuttujat ja kirjastot. Oletetaan, että haluamme tallentaa päivämäärän muuttujaan nimeltä "tänään", käyttäen RTC (Real-Time Clock) -kirjastoa.

```Arduino
#include <RTClib.h> //Ladataan RTC-kirjasto
RTC_DS3231 rtc; //Luodaan RTC-olio
DateTime today; //Luodaan muuttuja, johon tallennetaan päivämäärä

void setup() {
  Serial.begin(9600); //Aloitetaan sarjaliikenne
  rtc.begin(); //Käynnistetään RTC
}

void loop() {
  today = rtc.now(); //Haetaan nykyinen päivämäärä ja tallennetaan muuttujaan
  Serial.println(today); //Tulostetaan päivämäärä sarjaporttiin
  delay(1000); //Viiveen lisääminen estää päivämäärän näyttämisen liian nopeasti
}
```

Tämän koodin avulla saamme tulosteena nykyisen päivämäärän muodossa "YYYY-MM-DD HH:MM:SS". Voit myös muokata koodia lisäämällä määrittelemättömät muuttujat mm. "vuosi", "kuukausi", "päivä" ja "tunti", jotta voit näyttää ne erillisinä lukuina.

## Syvällisempi sukellus

RTC-kirjasto käyttää oletuksena mikropiirin sisäistä kellotietoa päivämäärän ja ajan saamiseksi. Voit kuitenkin myös liittää erillisen RTC-moduulin, jotta voit varmistaa tarkemman ja vakioajan. Tällöin sinun on muokattava koodia käyttämään tätä lisämoduulia.

```Arduino
#include <RTClib.h>
#include <Wire.h> //Tarvitaan RTC-moduulin käyttöön

RTC_DS3231 rtc(Wire); //Määritellään RTC-moduulin käyttö kirjastoon

DateTime today;

void setup() {
  Serial.begin(9600);
  Wire.begin(); //AVR-piirin aloitus RTC: lle
  rtc.begin(); //RTC-moduulin käynnistys
}

void loop() {
  today = rtc.now(); //Haetaan nykyinen päivämäärä RTC-moduulilta
  Serial.println(today);
  delay(1000);
}
```

Jos haluat tarkempia ohjeita RTC-moduulin käyttöön, suosittelemme tarkastelemaan RTC-kirjaston dokumentaatiota tai hakemaan lisätietoa verkosta.

## Katso myös

- [RTC-kirjaston dokumentaatio](https://github.com/adafruit/RTClib)
- [Arduino Reference - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Real Time Clocks ja Arduino - Gravity Academy](https://www.dfrobot.com/blog-945.html)