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

## ¿Por qué descargar una página web?

Descargar una página web puede ser útil para obtener información específica de un sitio en particular, como datos de precios o noticias actualizadas. También puede ser útil para crear un archivo de respaldo de una página web importante o para realizar pruebas y experimentos en su código fuente.

## Cómo hacerlo en Arduino

Para descargar una página web en Arduino, necesitarás utilizar la librería WiFiClient. Primero, debes conectar tu Arduino a una red WiFi. Luego, sigue estos pasos:

1. Incluir la librería WiFiClient en tu código:
```Arduino
#include <WiFiClient.h>
```

2. Crear un objeto de tipo `WiFiClient`:
```Arduino
WiFiClient client;
```

3. Establecer una conexión con el servidor web del sitio que deseas descargar:
```Arduino
if (client.connect("example.com", 80)) {
  // Código a ejecutar si la conexión es exitosa
} else {
  // Código a ejecutar si la conexión falla
}
```

4. Enviar una solicitud HTTP GET al servidor:
```Arduino
client.println("GET /index.html HTTP/1.1");
client.println("Host: example.com");
client.println("Connection: close");
client.println();
```

5. Leer y almacenar los datos recibidos del servidor utilizando el método `readString()`:
```Arduino
while (client.available()) {
  String data = client.readString();
  // Hacer algo con los datos recibidos
}
```

## Profundizando en la descarga de páginas web

Al descargar una página web utilizando Arduino, es importante tener en cuenta los siguientes puntos:

- Algunos servidores web pueden requerir una conexión segura (HTTPS) en lugar de una conexión no segura (HTTP). En ese caso, deberás utilizar la librería WiFiSSLClient en lugar de WiFiClient.
- Si estás descargando una página web que contiene imágenes u otros archivos, deberás utilizar otras librerías, como la librería SD, para almacenarlos en tu Arduino.
- Ten en cuenta que descargar una página web puede ser un proceso lento y consumir mucha memoria en tu Arduino, por lo que es importante optimizar tu código y evitar errores de memoria.
- Algunos servidores pueden bloquear las solicitudes de descarga si se realizan demasiado a menudo, así que asegúrate de revisar la política del sitio web antes de programar tu Arduino para descargar una página de forma repetitiva.

## Ver también

- [Documentación oficial de la librería WiFiClient](https://www.arduino.cc/en/Reference/WiFiClient)
- [Tutorial de descarga de páginas web con el ESP8266](https://randomnerdtutorials.com/esp8266-nodemcu-http-get-post-arduino/)
- [Ejemplo de descarga de una página web con Arduino y la librería WiFiClient](https://create.arduino.cc/projecthub/rowan07/reading-a-webpage-using-an-arduino-uno-62039a)