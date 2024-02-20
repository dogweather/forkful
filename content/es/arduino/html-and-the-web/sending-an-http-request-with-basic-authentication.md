---
date: 2024-01-20 18:00:57.211237-07:00
description: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica es el proceso\
  \ de mandar una petici\xF3n a un servidor que requiere un nombre de usuario y contrase\xF1\
  a\u2026"
lastmod: 2024-02-19 22:05:17.839455
model: gpt-4-1106-preview
summary: "Enviar una solicitud HTTP con autenticaci\xF3n b\xE1sica es el proceso de\
  \ mandar una petici\xF3n a un servidor que requiere un nombre de usuario y contrase\xF1\
  a\u2026"
title: "Enviando una solicitud http con autenticaci\xF3n b\xE1sica"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP con autenticación básica es el proceso de mandar una petición a un servidor que requiere un nombre de usuario y contraseña codificados en base64. Los programadores lo hacen para interactuar con APIs o servicios web seguros, para obtener o enviar datos mientras se verifica la identidad.

## Cómo se hace:

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
#include <Base64.h>

const char* ssid = "tu_SSID";
const char* password = "tu_contraseña";
const char* httpServer = "http://tuservidor.com";
const char* user = "tu_usuario";
const char* pass = "tu_contraseña_http";

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Conectando al WiFi...");
  }

  HTTPClient http;
  http.begin(httpServer);
  
  String auth = user + ":" + pass;
  auth = base64::encode(auth);
  http.addHeader("Authorization", "Basic " + auth);

  int httpCode = http.GET();
  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Error en la solicitud: " + http.errorToString(httpCode));
  }
  http.end();
}

void loop() {
  // Aquí el código que repetirá tu Arduino.
}
```

## Profundizando

Enviar una solicitud HTTP con autenticación básica no es nada nuevo; ha existido prácticamente desde los inicios del protocolo HTTP. Alternativas modernas incluyen OAuth y tokens JWT, que proporcionan una seguridad más robusta. La autenticación básica codifica simplemente el usuario y la contraseña con Base64, lo cual puede ser decodificado fácilmente si la conexión no está asegurada con SSL/TLS. Implementar autenticación básica en Arduino involucra usar la biblioteca `ESP8266HTTPClient` para manejar la conexión HTTP, y la biblioteca `Base64` para la codificación requerida.

## Ver además

- [Base64 Arduino Library](https://github.com/Densaugeo/base64_arduino)
- [HTTP Authentication](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)
- [Arduino and REST API Integration](https://create.arduino.cc/projecthub/arduino/projects/tag/rest%20api)
