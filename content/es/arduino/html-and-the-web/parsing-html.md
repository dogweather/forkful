---
aliases:
- /es/arduino/parsing-html/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:30.880280-07:00
description: "Analizar HTML en proyectos Arduino trata sobre extraer informaci\xF3\
  n de p\xE1ginas web. Los programadores hacen esto para permitir que sus dispositivos\
  \ Arduino\u2026"
lastmod: 2024-02-18 23:09:10.259258
model: gpt-4-0125-preview
summary: "Analizar HTML en proyectos Arduino trata sobre extraer informaci\xF3n de\
  \ p\xE1ginas web. Los programadores hacen esto para permitir que sus dispositivos\
  \ Arduino\u2026"
title: Analizando HTML
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Analizar HTML en proyectos Arduino trata sobre extraer información de páginas web. Los programadores hacen esto para permitir que sus dispositivos Arduino interactúen con Internet, recopilando datos de sitios web para propósitos que van desde la automatización del hogar hasta el monitoreo ambiental.

## Cómo hacerlo:

Analizar HTML en Arduino generalmente demanda bibliotecas de huella mínima debido a los limitados recursos del dispositivo. Una opción popular para el scraping y análisis web es usar las bibliotecas `ESP8266HTTPClient` y `ESP8266WiFi` para ESP8266, o sus equivalentes de ESP32, dado su soporte nativo para capacidades Wi-Fi y protocolos HTTP. Aquí hay un ejemplo básico para buscar y analizar HTML, asumiendo que estás trabajando con un ESP8266 o ESP32:

Primero, incluye las bibliotecas necesarias:
```cpp
#include <ESP8266WiFi.h> // Para ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// Usa las bibliotecas análogas de ESP32 si estás usando un ESP32

const char* ssid = "tuSSID";
const char* password = "tuCONTRASEÑA";
```

Conéctate a tu red Wi-Fi:
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("Conectando...");
    }
}
```

Haz una solicitud HTTP y analiza una pieza simple de HTML:
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //Verificar el estado de conexión de WiFi
        HTTPClient http;  //Declarar un objeto de la clase HTTPClient

        http.begin("http://example.com");  //Especificar el destino de la solicitud
        int httpCode = http.GET();  //Enviar la solicitud

        if (httpCode > 0) { //Verificar el código de retorno
            String payload = http.getString();   //Obtener la carga útil de respuesta de la solicitud
            Serial.println(payload);             //Imprimir la carga útil de la respuesta

            // Analizar una parte específica, por ejemplo, extraer título del payload
            int titleStart = payload.indexOf("<title>") + 7; // +7 para pasar la etiqueta "<title>"
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("Título de la Página: ");
            Serial.println(pageTitle);
        }

        http.end();   //Cerrar conexión
    }

    delay(10000); //Hacer una solicitud cada 10 segundos
}
```

Salida de muestra (asumiendo que http://example.com tiene una estructura HTML simple):
```
Conectando...
...
Título de la Página: Dominio de Ejemplo
```

Este ejemplo demuestra cómo buscar una página HTML y extraer el contenido de la etiqueta `<title>`. Para un análisis HTML más complejo, considera usar expresiones regulares (con cautela debido a las limitaciones de memoria) o funciones de manipulación de cadenas para navegar a través de la estructura HTML. El análisis avanzado puede requerir enfoques más sofisticados, incluyendo algoritmos de análisis personalizados adaptados a la estructura específica de HTML con la que estás tratando, ya que el entorno estándar de Arduino no incluye una biblioteca de análisis HTML incorporada.
