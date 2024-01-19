---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

Descargar una página web es básicamente cómo guardamos una copia de todo el contenido de un sitio web en local. Los programadores lo hacen para analizar la página, scrappear información, testear funcionalidades, entre otros propósitos.

## Cómo hacer:

Aquí te muestro un pedazo de código en Arduino para descargar una página web utilizando un módulo ESP8266:

```Arduino
#include <ESP8266WiFi.h>
#define SSID "nombre_de_tu_red"
#define PASSWORD "contraseña_de_tu_red"

WiFiClient client;

void setup() {
  Serial.begin(115200);
  WiFi.begin(SSID, PASSWORD);
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Conectando a la red WiFi...");
  }
  Serial.println("¡Conectado!");
}

void loop() {
  if (client.connect("www.pagina-ejemplo.com", 80)) {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.pagina-ejemplo.com");
    client.println("Connection: close");
    client.println();
  }
  while (client.available()) {
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```
Este código se conectará a la red WiFi que definas y descargará la página inicial de "www.pagina-ejemplo.com". El contenido de la página web se imprimirá en el monitor serial.

## Inmersión Profunda:

1. **Contexto histórico**: El primer intento de "crawler", un programa que descarga páginas para indexarlas, se cree que fue  el WebCrawler de 1994, que fue crucial en el desarrollo de Yahoo!. 
2. **Alternativas**: En lugar de ESP8266, puedes usar otros módulos como ESP32, que incluyen más funcionalidades.
3. **Detalles de implementación**: La descarga de una página web con Arduino se basa en realizar una petición GET a un servidor web. El servidor responde con el HTML de la página y lo lees con la función `client.readStringUntil('\r');`.

## Fuentes Relacionadas:

1. Documentación oficial de Arduino: https://www.arduino.cc/reference/en/
2. Tutorial de connectar Arduino a internet: https://internetdelascosas.cl/2015/03/23/tutorial-esp8266-conexión-a-internet-con-arduino-uno/
3. Información adicional sobre peticiones HTTP: https://desarrolloweb.com/articulos/2993.php
4. Tutorial para web scrapping: https://www.acamica.com/clases/1479/tutoriales/webscraping-con-python-y-beautifulsoup