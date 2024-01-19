---
title:                "Inviare una richiesta http"
html_title:           "C++: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Inviare una richiesta HTTP significa chiedere al server di trasmettere dati da un'ubicazione specifica. Questo consente ai programmatori di interagire con server Web, recuperare dati da vari servizi online e costruire applicazioni IoT (Internet delle cose) dinamiche.

## Come fare:

Ecco un esempio di come inviare una richiesta HTTP utilizzando il modulo ESP8266 con Arduino.

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID";
const char* password =  "your_PASSWORD";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting...");
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) {
    HTTPClient http;
    
    http.begin("http://api.openweathermap.org/data/2.5/weather?q=Rome,it&appid=your_api_key");
    int httpCode = http.GET();
    
    if (httpCode > 0) {
        String payload = http.getString();
        Serial.println(payload);
    }

    http.end();
  }

  delay(30000);
}
```

Ecco come appare l'output:
```
{
  "coord": {
    "lon": 12.4839,
    "lat": 41.8919
  },
  "weather": [{
    "id": 801,
    "main": "Clouds",
    "description": "few clouds",
    "icon": "02d"
  }],
  ...
}
```

## Approfondimenti:

Il concetto di richieste HTTP risale agli inizi del web, da quando il protocollo HTTP è stato definito nel 1991 come modo standard di comunicare su Internet. La creazione del IoT ha aperto un'enorme gamma di possibilità per l'uso di queste richieste.

Alternative all'uso diretto delle richieste HTTP esistono. MQTT è ad esempio un protocollo molto popolare per le comunicazioni IoT. Tuttavia, HTTP ha il vantaggio di essere universalmente supportato.

Nell'esempio di codice fornito, utilizziamo la libreria ESP8266HTTPClient. Questa libreria incapsula la complessità del basso livello delle richieste HTTP rendendo relativamente facile per noi fare richieste.

## Vedi anche:

1. Documentazione ufficiale di Arduino sulla libreria WiFi: https://www.arduino.cc/en/Reference/WiFi
2. Documentazione ufficiale di ESP8266HTTPClient: https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient.html
3. Introduzione ad HTTP: https://developer.mozilla.org/it/docs/Web/HTTP/Overview
4. Introduzione all'IoT con Arduino: https://www.arduino.cc/en/digital/HomePage