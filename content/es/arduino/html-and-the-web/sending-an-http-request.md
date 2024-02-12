---
title:                "Enviando una solicitud http"
aliases:
- /es/arduino/sending-an-http-request/
date:                  2024-01-20T17:59:06.620091-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Enviar una petición HTTP es básicamente pedirle datos a una página web o servidor. Lo hacemos para interactuar con servicios web, sea para recoger información o enviarla.

## Cómo hacerlo:
El siguiente código usa la biblioteca ESP8266WiFi para un módulo ESP8266, pero es similar para otros módulos:

```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "tuSSID";
const char* password = "tuContraseña";
const char* host = "api.ejemplo.com";

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.print("Conectado a ");
  Serial.println(ssid);
  
  if (!client.connect(host, 80)) {
    Serial.println("Conexión fallida");
    return;
  }
  
  client.print(String("GET /ruta HTTP/1.1\r\n") +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");
  unsigned long timeout = millis();
  while (client.available() == 0) {
    if (millis() - timeout > 5000) {
      Serial.println("Cliente Timeout!");
      client.stop();
      return;
    }
  }
  
  while(client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
}
```
Al ejecutarlo, verás la respuesta HTTP en el puerto serial.

## Profundizando:
Antes de Arduino y ESP, el envío de peticiones HTTP era territorio de computadoras y servidores. Ahora, microcontroladores compactos gestionan estas tareas fácilmente. Existen varias bibliotecas y métodos: `ESP8266HTTPClient` y `WiFiClientSecure` para HTTPS, por ejemplo. La elección depende del módulo y tus necesidades.

A nivel de implementación, debes considerar la seguridad (HTTPS vs HTTP), el manejo de fallos de conexión y estructuras de respuesta JSON/XML si tu API comunica datos complejos.

## Ver También:
- [Documentación de ESP8266WiFi](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
- [API de referencia de Arduino](https://www.arduino.cc/reference/en/)
