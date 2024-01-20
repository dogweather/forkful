---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?
El envío de una solicitud HTTP con autenticación básica en Arduino implica el envío de datos al servidor para verificar quién eres. Los programadores lo hacen para establecer una conexión segura con servidores web y otros dispositivos en línea.

## ¿Cómo hacerlo?
Antes de nada, necesitarás la biblioteca ArduinoHttpClient. Instálala a través del Gestor de Bibliotecas en el IDE de Arduino.

```Arduino
#include <ArduinoHttpClient.h>
#include <Ethernet.h>
#include <SPI.h>
```

Definimos las variables necesarias:
```Arduino
char server[] = "www.tuservidor.com";
char auth[] = "dXNlcjpwYXNz";
EthernetClient ethernet;
HttpClient cliente = HttpClient(ethernet, server);
```

Para enviar una solicitud HTTP GET con autenticación básica, usamos el siguiente código:

```Arduino
cliente.beginRequest();
cliente.get("/ruta");
cliente.sendBasicAuth("usuario", "contraseña");
cliente.endRequest();

// lee la respuesta
int statusCode = cliente.responseStatusCode();
String respuesta = cliente.responseBody();
```

## Un Vistazo en Detalle
Esta forma de autenticación se introdujo en los primeros días de la web. Sin embargo, ha sido reemplazada en gran medida por métodos más seguros como OAuth y OpenID en aplicaciones web modernas. Sin embargo, la autenticación básica todavía se usa con frecuencia en el control de dispositivos IoT, como Arduino.

Una alternativa para enviar una solicitud HTTP con autenticación básica es utilizar la biblioteca ESP8266HTTPClient en lugar de ArduinoHttpClient, cuya implementación es parecida pero esta se utiliza específicamente con ESP8266.

Un detalle de implementación a tener en cuenta es que la autenticación básica no es segura por sí misma. Las credenciales se envían como texto sin cifrar, lo que podría ser interceptado fácilmente. Para garantizar la seguridad de la autenticación básica, siempre debes usarla con un protocolo seguro, como HTTPS.

## Ver También
ArduinoHttpClient: http://www.arduino.cc/en/Reference/ArduinoHttpClient
ESP8266HTTPClient: https://github.com/esp8266/Arduino/tree/master/libraries/ESP8266HTTPClient
HTTPS con Arduino: https://create.arduino.cc/projecthub/electropeak/esp8266-and-the-arduino-ide-part-2nd-control-led-from-webserver-929577