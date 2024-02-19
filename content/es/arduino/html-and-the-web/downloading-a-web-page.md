---
aliases:
- /es/arduino/downloading-a-web-page/
date: 2024-01-20 17:43:16.533112-07:00
description: "Descargar una p\xE1gina web es b\xE1sicamente pedirle a un servidor\
  \ de internet que nos mande los datos de una p\xE1gina. Los programadores hacen\
  \ esto para obtener\u2026"
lastmod: 2024-02-18 23:09:10.260329
model: gpt-4-1106-preview
summary: "Descargar una p\xE1gina web es b\xE1sicamente pedirle a un servidor de internet\
  \ que nos mande los datos de una p\xE1gina. Los programadores hacen esto para obtener\u2026"
title: "Descargando una p\xE1gina web"
---

{{< edit_this_page >}}

## Qué & Por Qué?
Descargar una página web es básicamente pedirle a un servidor de internet que nos mande los datos de una página. Los programadores hacen esto para obtener información útil, actualizar datos en tiempo real o interactuar con servicios web.

## Cómo hacerlo:
Para descargar una página web con Arduino, necesitas un módulo de red como el Ethernet Shield o un módulo WiFi. Acá un código de ejemplo usando WiFi:

```Arduino
#include <WiFi.h>

const char* ssid     = "Tu_SSID";
const char* password = "Tu_Contraseña";

const char* host = "example.com";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }

  Serial.println("WiFi conectado");
  Serial.println("IP address: ");
  Serial.println(WiFi.localIP());

  delay(1000);

  Serial.print("Conectando a ");
  Serial.println(host);

  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(host, httpPort)) {
    Serial.println("Conexión fallida");
    return;
  }

  String url = "/pagina_que_quieres_descargar";
  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");

  while (client.available() == 0) {
    delay(1000);
  }

  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
  // No necesitamos nada aquí.
}
```

## Inmersión Profunda:
El concepto de descargar páginas web no es nuevo, data de los inicios de la World Wide Web. Empezamos con conexiones telnet, pasamos por HTTP/1.0, luego al más eficiente HTTP/1.1, y ahora usamos protocolos aún más rápidos como HTTP/2. Alternativamente, en lugar de WiFi, podrías usar Ethernet o módulos celulares, dependiendo de tu proyecto. La implementación dependerá del módulo de hardware específico que utilices, así que asegúrate de consultar la documentación correspondiente.

## Ver También:
- Documentación de la biblioteca WiFi de Arduino: https://www.arduino.cc/en/Reference/WiFi
- Código de ejemplo para Ethernet Shield: https://www.arduino.cc/en/Tutorial/WebClient
- Información sobre HTTP/2: https://developers.google.com/web/fundamentals/performance/http2
