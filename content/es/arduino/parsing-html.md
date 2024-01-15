---
title:                "Analizando html"
html_title:           "Arduino: Analizando html"
simple_title:         "Analizando html"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/parsing-html.md"
---

{{< edit_this_page >}}

## ¿Por qué?
¿Alguna vez has querido extraer información específica de una página web? Ya sea para utilizarla en tu proyecto de Arduino o simplemente para recopilar datos, el análisis de HTML puede ser una herramienta útil para obtener los datos que necesitas. En este artículo, te enseñaremos cómo realizar este proceso utilizando Arduino.

## Cómo hacerlo
Para comenzar, necesitarás un Arduino y una conexión a Internet. También es útil tener conocimientos básicos de HTML y cómo funciona la estructura de una página web.

Primero, debes descargar e instalar la biblioteca "ESP8266HTTPClient.h". Esta biblioteca te permitirá realizar solicitudes HTTP utilizando tu módulo WiFi.

Luego, utilizando la función "HTTPClient", puedes realizar una solicitud a una página web específica. Por ejemplo, si queremos obtener la temperatura actual de una ciudad, podríamos utilizar la API de OpenWeatherMap. El código se vería algo así:

```
#include <ESP8266HTTPClient.h>

void setup() {
  Serial.begin(115200); // Iniciamos la comunicación serial
  WiFi.begin("nombre de tu red WiFi", "contraseña"); // Conectamos a la red WiFi
  while (WiFi.status() != WL_CONNECTED) { // Esperamos a que se conecte
    delay(500);
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) { // Si estamos conectados a la red
    HTTPClient http; // Creamos una instancia de HTTPClient
    http.begin("http://api.openweathermap.org/data/2.5/weather?id=524901&appid=API_KEY"); // Realizamos la solicitud a la URL especificada
    int httpCode = http.GET(); // Obtenemos el código de respuesta
    if (httpCode > 0) { // Si existe una respuesta
      String payload = http.getString(); // Guardamos los datos de la respuesta en una variable
      Serial.println(payload); // Imprimimos los datos en la consola serial
    }
    http.end(); // Terminamos la conexión
  }
  delay(60000); // Esperamos 1 minuto antes de realizar otra solicitud
}
```

En este ejemplo, estamos solicitando los datos de la ciudad de Moscú (identificado con "id=524901") utilizando una API key de OpenWeatherMap. Una vez que tenemos los datos, los imprimimos en la consola serial cada 1 minuto.

Este es solo un ejemplo básico de cómo puedes utilizar Arduino para obtener y analizar datos de una página web. Puedes adaptar este código a tus necesidades y realizar solicitudes a diferentes APIs o páginas web.

## Profundizando
Si deseas profundizar en el análisis de HTML, hay bibliotecas disponibles que te permiten analizar el contenido de una página web y extraer información específica. Algunas de estas bibliotecas son "ESP8266WebServer.h" y "ESP8266WiFiServer.h". Con estas bibliotecas, puedes crear un servidor web en tu Arduino y utilizarlo para mostrar o enviar datos a través de una página web.

También puedes explorar la posibilidad de utilizar Arduino para raspar (scraping) datos de páginas web, lo que implica analizar y extraer información de una gran cantidad de páginas. Sin embargo, es importante tener en cuenta que algunas páginas web pueden prohibir esta práctica, así que asegúrate de comprobar las políticas de cada página antes de realizar cualquier análisis.

## Ver también
- [Página oficial de Arduino](https://www.arduino.cc/)
- [Documentación de la biblioteca ESP8266HTTPClient.h](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/http-client.html)
- [Documentación de la biblioteca ESP8266WebServer.h](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/esp8266webserver.html)
- [Documentación de la biblioteca ESP8266WiFiServer.h](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/esp8266wifi-server.html)