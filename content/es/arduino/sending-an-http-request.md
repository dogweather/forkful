---
title:                "Enviando una solicitud http."
html_title:           "Arduino: Enviando una solicitud http."
simple_title:         "Enviando una solicitud http."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es una forma en la que los programadores pueden comunicarse con servidores en la web. Es una forma de que su programa reciba información o realice acciones a través de Internet.

## Cómo:

Escribir una solicitud HTTP en Arduino es fácil. Primero, asegúrate de tener la biblioteca WiFiClient.h incluida en tu programa. Luego, crea un objeto de tipo WiFiClient y usa su método .connect() para conectarte a la URL deseada. A continuación, escribe tu solicitud usando el formato de solicitud HTTP adecuado y usa el método .print() para enviarla al servidor. Por último, usa el método .read() para recibir la respuesta del servidor y procesarla en tu programa.

```Arduino
#include <WiFiClient.h>

WiFiClient client;
client.connect("www.ejemplo.com", 80); //conexión a la URL y puerto 80
client.print("GET /index.html HTTP/1.1\r\n"); //solicitud GET para la página "index.html"
client.print("Host: www.ejemplo.com\r\n"); //se proporciona el nombre del servidor
client.print("Connection: close\r\n\r\n"); //se cierra la conexión después de recibir la respuesta
String respuesta = client.readString(); //lee y almacena la respuesta del servidor
```

## Profundizando:

Las solicitudes HTTP se han utilizado desde los inicios de la web para facilitar la comunicación entre servidores y clientes. Aunque ahora existen otras alternativas, como las solicitudes HTTPS más seguras, las solicitudes HTTP siguen siendo una forma eficaz de comunicarse con servidores.

Además de enviar solicitudes GET, como se muestra en el ejemplo anterior, también se pueden enviar solicitudes POST, PUT y DELETE utilizando el mismo formato de solicitud y los métodos .print() y .write() adecuados. También es importante tener en cuenta que algunos servidores pueden requerir ciertos encabezados en la solicitud para que funcione correctamente.

## Ver también:

- [Documentación oficial de Arduino sobre WiFiClient] (https://www.arduino.cc/en/Reference/WiFiClient/)
- [Tutorial de Adafruit sobre cómo usar solicitudes HTTP en Arduino] (https://learn.adafruit.com/adafruit-io-basics-simple-internet-of-things-dashboard/arduino-http-requests)
- [Especificación de HTTP por World Wide Web Consortium] (https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html)