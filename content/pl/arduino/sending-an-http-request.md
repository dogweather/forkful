---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP to jedna z podstawowych umiejętności każdego programisty. Polega ona na komunikacji z serwerem internetowym w celu uzyskania danych lub wykonania jakiejś akcji. Jest to niezbędna funkcjonalność w wielu projektach, więc warto nauczyć się jej obsługi.

## Jak to zrobić?
Arduino oferuje prosty sposób na wysyłanie żądań HTTP za pomocą biblioteki Ethernet. Poniżej przedstawiamy przykład kodu, który wysyła żądanie GET do serwera i wyświetla odpowiedź w konsoli.

```
#include <Ethernet.h>
#include <SPI.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED }; //adres MAC płytki
byte ip[] = { 192, 168, 1, 177 }; //adres IP płytki
byte server[] = { 192, 168, 1, 100 }; //adres IP serwera
EthernetClient client; //tworzymy obiekt klienta

void setup() {
  Ethernet.begin(mac, ip); //inicjalizacja połączenia
  Serial.begin(9600); //inicjalizacja komunikacji z konsolą
  delay(1000); //czekamy na połączenie
  Serial.println("connecting..."); 
}

void loop() {
  if (client.connect(server, 80)) { //nawiązanie połączenia z serwerem na porcie 80
    Serial.println("connected");
    client.println("GET / HTTP/1.0"); //wysłanie żądania GET
    client.println("Host: 192.168.1.100"); //adres hosta
    client.println(); //pusta linia na koniec
  }
  else {
    Serial.println("connection failed"); //błąd połączenia
  }
  delay(5000); //odpoczynek przez 5 sekund
}
```

Po uruchomieniu powyższego kodu w konsoli pojawi się odpowiedź od serwera, zawierająca kod stanu oraz inne informacje, takie jak nagłówki itp. Teraz możesz zaprogramować swoje urządzenie tak, aby przetwarzało otrzymane dane lub wykonywało inne akcje.

## Deep Dive
Wysyłanie żądań HTTP stało się standardowym sposobem komunikacji z serwerami internetowymi. Jest to powszechnie stosowane w aplikacjach webowych, IoT oraz innych projektach wymagających dostępu do zasobów sieciowych. Alternatywne metody to między innymi protokół MQTT lub komunikacja bezpośrednio przez port szeregowy.

Warto również wspomnieć, że obecnie biblioteka Ethernet jest faktycznie przestarzała, a zalecane jest korzystanie z nowocześniejszych i lepiej udokumentowanych rozwiązań, takich jak biblioteka ESP8266 lub ESP32. Oprócz tego, można również wykorzystać gotowe moduły Internetu Rzeczy, takie jak Raspberry Pi czy Particle.

## Zobacz także
- [Przykład komunikacji Arduino z serwerem przez WiFi](https://www.arduino.cc/en/Tutorial/WiFiWebClient)
- [Biblioteka ESP8266 do obsługi protokołu HTTP](https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient)
- [Projekt Internetu Rzeczy z wykorzystaniem Particle](https://www.hackster.io/particle/products/particle-photon/watches/417)