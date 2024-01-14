---
title:                "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Enviar solicitudes HTTP con autenticación básica es una herramienta valiosa para comunicarse con servidores y acceder a recursos en línea. Puede ser utilizado en proyectos de IoT para obtener y enviar datos, también puede ser útil para acceder a APIs externas.

## ¿Cómo hacerlo?

Primero, necesitamos incluir la librería WiFiClientSecure.h para acceder a funciones de cliente HTTP seguras. Luego, debemos crear un objeto de cliente WiFi y establecer la conexión a la red WiFi correspondiente. Luego, podemos crear un objeto de cliente HTTP para hacer la solicitud y configurarlo para utilizar autenticación básica con nuestro nombre de usuario y contraseña.

```
Arduino #include <WiFiClientSecure.h>

// Conexión WiFi
WiFiClientSecure wifiClient;
WiFi.begin("nombre_de_red", "contraseña");

// Configuración del cliente HTTP
WiFiClientSecure http;
http.setAuthorization("nombre_de_usuario", "contraseña");
```

Luego, podemos crear la solicitud HTTP utilizando el método "GET" y especificando la URL del recurso al que queremos acceder.

```
// Solicitud HTTP GET
http.get(URL);
```

Finalmente, podemos enviar la solicitud y obtener la respuesta del servidor utilizando los métodos "send" y "readString".

```
// Envío de solicitud
http.send();

// Obtener respuesta del servidor
String respuesta = http.readString();
```

La respuesta del servidor contendrá la información solicitada, que podemos utilizar en nuestro código para cualquier propósito que necesitemos.

## Profundizando

Al enviar una solicitud HTTP con autenticación básica, estamos utilizando el protocolo de seguridad HTTPS para garantizar la privacidad de nuestras credenciales de autenticación. HTTPS utiliza un certificado de seguridad que verifica la identidad del servidor y protege la comunicación utilizando encriptación.

También es importante tener en cuenta que, al utilizar autenticación básica, nuestros nombres de usuario y contraseñas se envían sin encriptación a través de la red. Por lo tanto, es importante utilizar una conexión segura como HTTPS para proteger nuestras credenciales de posibles ataques malintencionados.

## Ver también

- Librería WiFiClientSecure.h: http://www.arduino.cc/en/Reference/WiFiClientSecure
- Código de ejemplo para enviar solicitudes HTTP con autenticación básica: https://circuits4you.com/2019/01/11/nodemcu-esp8266-get-post-request/