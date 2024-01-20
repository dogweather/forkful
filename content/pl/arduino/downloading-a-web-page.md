---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Ściąganie strony internetowej polega na pobieraniu kodu HTML danej strony, przez co programiści mogą analizować i manipulować te dane. Robimy to, aby zautomatyzować procesy, takie jak zbieranie danych (web scraping) lub monitorowanie zmian na stronach internetowych.

## Jak to zrobic:

Poniżej znajduje się przykład wykorzystania biblioteki Ethernet do pobrania strony internetowej za pomocą Arduino. 

```Arduino
#include <Ethernet.h>

byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
EthernetClient client;

void setup() {
  Ethernet.begin(mac);
  delay(1000);
  
  if (client.connect("www.przykladowastrona.pl", 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.przykladowastrona.pl");
    client.println("Connection: close");
    client.println();
  }
}

void loop() {
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }
  if (!client.connected()) {
    client.stop();
    for(;;);
  }
}
```

Po zagłębieniu się w proces, obejrzyj wynik na podłączonym monitorze szeregowej. Zobaczysz pełną zawartość strony internetowej.

## Głębsze spojrzenie:

Pobieranie stron internetowych na Arduino ma swój początek wraz ze wsparciem dla Ethernetu. Są też inne alternatywy - Wi-Fi (biblioteka WiFiNINA), GSM (biblioteka GSM). Wybór zależy od rodzaju dostępnego sprzętu i wymagań projektu.

Implementacja pobierania strony web poprzez Arduino jest całkiem prosta, ale ma swoje ograniczenia. Arduino ma ograniczoną pamięć, dlatego musimy dbać o to, aby nie przekroczyć dostępnej ilości podczas pracy z dużymi stronami.

## Zobacz także:

Dla głębszego zrozumienia, przejrzyj następujące źródła:

- Więcej o bibliotece Ethernet: [oficjalna dokumentacja](https://www.arduino.cc/en/Reference/Ethernet)
- Więcej o bibliotece WiFiNINA: [oficjalna dokumentacja](https://www.arduino.cc/en/Reference/WiFiNINA)
- Więcej o bibliotece GSM: [oficjalna dokumentacja](https://www.arduino.cc/en/Reference/GSM)