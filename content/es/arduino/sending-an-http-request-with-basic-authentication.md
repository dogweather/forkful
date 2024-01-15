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

## ¿Por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma segura de acceder a recursos en línea, como bases de datos o servicios web, que requieren una identificación para acceder. Esto es esencial para proteger la información privada y garantizar que solo los usuarios autorizados puedan acceder a ella.

## Cómo hacerlo

```Arduino
#include <WiFiClientSecure.h> //Librería para conexiones seguras
#include <WiFiNINA.h> //Librería para conexión WiFi

char ssid[] = "nombre de red wifi"; //Nombre de la red WiFi a la que te quieres conectar
char pass[] = "contraseña"; //Contraseña de la red WiFi
char server[] = "url del servidor"; //URL del servidor al que queremos enviar la solicitud
int port = 443; //El puerto por defecto para conexiones seguras es el 443

void setup() {
  WiFi.begin(ssid, pass); //Conexión a la red WiFi
  while (WiFi.status() != WL_CONNECTED) { //Esperar hasta que se establezca la conexión
    delay(500);
  }
}

void loop() {
  if (WiFi.status() == WL_CONNECTED) { //Si la conexión está establecida
    WiFiClientSecure client; //Crear un cliente WiFi seguro
    if (!client.connect(server, port)) { //Conexión al servidor
      Serial.println("No se pudo conectar al servidor");
    } else {
      Serial.println("Conectado al servidor");
      String auth = "usuario:contraseña"; //Usuario y contraseña separados por ":" para la autenticación básica
      String base64 = base64::encode(auth); //Codificar en base64 la cadena usuario:contraseña
      String header = "Authorization: Basic " + base64 + "\r\n"; //Crear el encabezado de autenticación
      client.println("GET /ruta/api HTTP/1.0"); //Enviar la solicitud, busca el recurso /ruta/api en el servidor
      client.println("Host: servidor.com"); //Especificar el servidor
      client.println(header); //Enviar el encabezado de autenticación
      client.println(); //Finalizar la solicitud
      while (client.connected()) { //Esperar hasta que la conexión se complete
        String line = client.readStringUntil('\n'); //Leer la respuesta del servidor
        Serial.print(line); //Imprimir en el monitor serial
      }
      client.stop(); //Cerrar la conexión
    }
  }
  delay(1000); //Esperar 1 segundo antes de volver a enviar la solicitud
}
```

Este ejemplo utiliza la librería WiFiClientSecure para establecer una conexión segura y enviar una solicitud HTTP con autenticación básica a un servidor específico. Primero, se establece una conexión a través de WiFi, luego se crea un cliente seguro y se envía una solicitud GET especificando el recurso que se desea obtener en el servidor. La autenticación se realiza mediante la codificación en base64 del usuario y la contraseña en un encabezado específico.

## Profundizando en el envío de una solicitud HTTP con autenticación básica

La autenticación básica es un método simple pero efectivo para proteger recursos en línea. Sin embargo, no es tan seguro como otros métodos más complejos, por lo que se recomienda utilizarlo junto con otras medidas de seguridad. Además, es importante tener en cuenta que la autenticación básica envía las credenciales de acceso en texto claro, lo que significa que pueden ser interceptadas por terceros. Por lo tanto, se recomienda utilizar una conexión segura (HTTPS) al enviar solicitudes con autenticación básica.

## Ver también

- [Librería WiFiClientSecure](https://www.arduino.cc/en/Reference/WiFiClientSecure)
- [Librería WiFiNINA](https://www.arduino.cc/en/Reference/WiFiNINA)
- [Tutorial de autenticación básica en HTTP](https://www.tutorialspoint.com/http/http_authentication.htm)
- [Tutorial de base64 en Arduino](https://techtutorialsx.com/2017/07/24/arduino-esp32-base64-encode/)