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

### ¿Qué & Por qué? 
Enviar una solicitud HTTP con autenticación básica es una forma de proteger información confidencial al comunicarse con un servidor. Los programadores pueden utilizar esto para garantizar que solo los usuarios autorizados puedan acceder y enviar datos a su servidor.

### Cómo:
Utilizar la función `sendAuthenticationRequest()` para enviar una solicitud HTTP con autenticación básica en un Arduino. Asegúrese de incluir la biblioteca `WiFiClient.h` y establezca las credenciales del servidor en las variables `username` y `password`. Aquí hay un ejemplo de código y su posible resultado:

```
#include <WiFiClient.h>

const char* username = "usuario";
const char* password = "contraseña";

void setup() {
  //Inicializar conexión WiFi
  Serial.begin(9600);
  delay(1000);
  Serial.println("\nConectando a la red WiFi...");
  WiFi.begin(ssid, pass);

  //Esperar a que se establezca la conexión
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(". ");
  }
  Serial.println("\n¡Conexión establecida!\n");
}

// Función para enviar la solicitud HTTP con autenticación básica
void sendAuthenticationRequest() {
  WiFiClient client;
  const int httpPort = 80;
  if (!client.connect(server, httpPort)) {
    Serial.println("La conexión ha fallado.");
    return;
  }

  //Solicitud HTTP
  client.print(String("GET /data HTTP/1.1\r\n") +
               "Host: " + server + "\r\n" +
               "Authorization: Basic " + base64Authorization + "\r\n" +
               "Connection: close\r\n\r\n");

  //Esperar a la respuesta del servidor
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    if (line == "\r") {
      break;
    }
  }
  Serial.println("Solicitud enviada con éxito.");
  client.stop();
}

void loop() {
  sendAuthenticationRequest();
  delay(5000); //Enviar solicitud cada 5 segundos
}
```

Posible resultado en el monitor serial:
```
Conectando a la red WiFi...
¡Conexión establecida!

Solicitud enviada con éxito.
```

### Deep Dive:
Esta forma de autenticación básica se remonta a la década de 1990 y es una forma simple pero efectiva de proteger una conexión entre un cliente y un servidor. Otras formas de autenticación, como la autenticación de clave pública, son más seguras pero también más complejas de implementar en un Arduino.

Además, asegúrese de que su servidor tenga configurada la autenticación básica y que las credenciales coincidan con las establecidas en su código de Arduino.

### Ver también:
- [Tutorial de Adafruit sobre autenticación básica en Arduino](https://learn.adafruit.com/esp8266-temperature-slash-humidity-webserver/basic-http-auth)
- [Documentación oficial de Arduino sobre la función `WiFiClient`](https://www.arduino.cc/en/Reference/WiFiClient)