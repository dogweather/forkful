---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:06.322117-07:00
description: "Een webpagina downloaden betekent het ophalen van de HTML-inhoud van\
  \ de URL waar je naar kijkt. Programmeurs doen dit om gegevens te trekken, hun gadgets\u2026"
lastmod: '2024-03-13T22:44:51.071433-06:00'
model: gpt-4-0125-preview
summary: "Een webpagina downloaden betekent het ophalen van de HTML-inhoud van de\
  \ URL waar je naar kijkt. Programmeurs doen dit om gegevens te trekken, hun gadgets\u2026"
title: Een webpagina downloaden
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent het ophalen van de HTML-inhoud van de URL waar je naar kijkt. Programmeurs doen dit om gegevens te trekken, hun gadgets te updaten, of simpelweg om het internet te gebruiken voor meer dan alleen kattenvideo's.

## Hoe:

Hier is de kern van de zaak: laat je Arduino surfen op het web en pak wat je nodig hebt.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "jouwSSID";
const char* wachtwoord = "jouwWACHTWOORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, wachtwoord);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Verbinden met WiFi...");
  }

  HTTPClient http;
  http.begin("http://example.com"); // Wissel met jouw URL
  
  int httpCode = http.GET();
  
  if (httpCode > 0) {
    if (httpCode == HTTP_CODE_OK) {
      String payload = http.getString();
      Serial.println(payload);
    }
  } else {
    Serial.printf("Fout in HTTP-verzoek: %s\n", http.errorToString(httpCode).c_str());
  }
  http.end();
}

void loop() {
  // Voor nu niets hier.
}
```

Schakel het in, en je zou de HTML van de webpagina in de Seriële Monitor moeten zien. Onthoud, je hebt het ESP8266 Wi-Fi module en een verbinding nodig.

## Diepgaand

Er was eens, toen waren Arduinos eenvoudige offline wezens. Toen kwamen er shields en modules die ze verbonden met het grote boze web. ESP8266 is een dergelijk magisch apparaatje, dat je Arduino verandert in een internet surfer.

Alternatieven? Zeker. Er zijn de ESP32, Ethernet Shield, en anderen voor dezelfde klus.

De kwaliteit van je internetverbinding, robuustheid van je stroomvoorziening, en zelfs het tijdstip van de dag kunnen de balans bepalen hoe goed je Arduino die pagina downloadt. We houden echt rekening met meer factoren dan alleen het schrijven van strakke code.

## Zie Ook

Wil je meer ontdekken? Bekijk deze dan:

- [Arduino Netwerken](https://www.arduino.cc/en/Guide/ArduinoEthernetShield)
- [ESP8266 GitHub Wiki](https://github.com/esp8266/Arduino)
- [ESP32 GitHub Repo](https://github.com/espressif/arduino-esp32)
