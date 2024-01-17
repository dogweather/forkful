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

## ¿Qué & Por qué?
Descargar una página web es el proceso de obtener datos de una página web y guardarlos en nuestra computadora o dispositivo. Los programadores lo hacen para acceder a la información que necesitan para sus proyectos o para automatizar tareas en sus dispositivos.

## Cómo:
El siguiente código en Arduino demuestra cómo descargar una página web:

```
Arduino
#include <WiFi.h>
#include <HTTPClient.h>

// Establecer la red WiFi a la que te quieres conectar
const char* ssid = "nombre_de_tu_red";
const char* password = "contraseña_de_tu_red";

void setup() {
    // Conectar a la red WiFi
    WiFi.begin(ssid, password);
    
    while (WiFi.status() != WL_CONNECTED) {
        delay(500);
        Serial.println("Conectando a WiFi..");
    }

    // Crear un objeto de HTTP
    HTTPClient http;
    // Ingresar la URL de la página web que se desea descargar
    http.begin("https://www.ejemplo.com");

    // Realizar la petición GET y guardar la respuesta en una variable
    int httpResponseCode = http.GET();
    String response = http.getString();

    // Imprimir la respuesta en la consola
    Serial.println(httpResponseCode);
    Serial.println(response);

    // Cerrar la conexión
    http.end();
}

void loop() {

}
```

## Profundizando:
Históricamente, descargar una página web era un proceso más lento y complicado, pero con el avance de la tecnología esto se ha vuelto más sencillo y rápido. Una alternativa para descargar una página web en Arduino es utilizar un módulo Ethernet o WiFi que permita una conexión directa a internet. Asimismo, también es importante tener en cuenta que no todas las páginas web permiten descargar su contenido o pueden tener restricciones de seguridad que impiden su descarga.

## Ver también:
- Documentación de Arduino: https://www.arduino.cc/en/Reference/WiFi
- Ejemplo de descarga de página web en ESP32: https://randomnerdtutorials.com/esp32-http-get-post-arduino/