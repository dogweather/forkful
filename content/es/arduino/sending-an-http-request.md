---
title:                "Enviando una solicitud http"
html_title:           "Arduino: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has querido controlar tu proyecto de Arduino desde una ubicación remota? En lugar de limitarte a las interacciones locales, un HTTP request te permite comunicarte con tu Arduino a través de internet. ¡Conecta tu placa al mundo entero y toma el control desde cualquier lugar!

## Cómo hacerlo

Para enviar un HTTP request desde tu Arduino, necesitas una conexión a internet y una librería llamada "WiFiClientSecure". Aquí hay un ejemplo de cómo puedes enviar un request GET utilizando la URL y la clave de API de una página web:

```
Arduino<!-- language: cpp -->

WiFiClientSecure client; //Inicializa el cliente
const char* host = "www.example.com"; //URL del sitio web
const int httpsPort = 443; //Puerto seguro HTTPS
String apiKey = "abcd1234"; //Clave de API proporcionada por el sitio web

if (!client.connect(host, httpsPort)) { //Intenta conectar al servidor
    Serial.println("Connection failed");
    return;
}

//Forma el request GET con la URL y la clave de API
String getRequest = "GET /getData?key=" + apiKey + " HTTP/1.1\r\n" +
                    "Host: www.example.com\r\n" +
                    "User-Agent: ArduinoWiFi/1.1\r\n" +
                    "Connection: close\r\n\r\n";
                    
client.print(getRequest); //Envía el request al servidor

while (client.connected()) { //Lee y muestra la respuesta del servidor
    String line = client.readStringUntil('\n');
    if (line == "\r") {
        Serial.println("Headers received");
        break;
    }
}
while (client.available()) {
    String line = client.readStringUntil('\n');
    Serial.print(line);
}
client.stop(); //Cierra la conexión
```

La salida en el monitor serial debería ser como esta:

```
HTTP/1.1 200 OK
Date: Tue, 15 Jun 2021 00:00:00 GMT
Content-Type: text/html; charset=utf-8

¡Tu solicitud fue exitosa! Aquí está tu dato: 123
```

## Exploración en profundidad

Este es solo un ejemplo básico, pero hay muchas más cosas que puedes hacer con HTTP requests en Arduino. Por ejemplo, podrías enviar datos de sensores a una página web para monitorearlos en tiempo real o incluso controlar cosas como luces o motores. También puedes utilizar diferentes tipos de requests, como POST o PUT. ¡Las posibilidades son infinitas!

## Ver también

- [Tutorial de HTTP con Arduino y WiFiClienteSecure](https://randomnerdtutorials.com/esp32-http-get-post-arduino/)
- [Documentación de la librería WiFiClientSecure](https://www.arduino.cc/en/Reference/WiFiClientSecure)
- [Ejemplos de proyectos de Arduino con HTTP requests](https://create.arduino.cc/projecthub/projects/tags/http%20request)