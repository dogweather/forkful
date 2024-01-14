---
title:                "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Por qué

La descarga de páginas web es esencial para conectarse con información en línea y acceder a sitios web en cualquier momento y en cualquier lugar. Con la ayuda de Arduino, es posible automatizar este proceso y tener una forma conveniente de obtener información cuando se necesite.

##Cómo

Para descargar una página web usando Arduino, primero debemos asegurarnos de tener una conexión a internet estable. Luego, utilizaremos la biblioteca "WiFiClient" para establecer una conexión con la URL del sitio web que queremos descargar. Una vez que hemos establecido la conexión, utilizaremos la función "print" para enviar una solicitud HTTP GET al sitio web y "readString" para recibir la respuesta del servidor. Finalmente, imprimiremos la respuesta en el monitor serial.

```
Arduino WiFiClient client;

if (client.connect(URL)) {
  Serial.println("Conexión establecida");
  client.print("GET / HTTP/1.1\r\n");
  client.print("Host: www.ejemplo.com\r\n");
  client.print("Connection: close\r\n\r\n");
}

while (client.available()) {
  String response = client.readString();
  Serial.println(response);
}
```

##Deep Dive

La función "print" envía la solicitud HTTP GET al servidor y le dice al servidor qué página queremos descargar. La forma en que se envía esta solicitud es mediante el uso de una cadena de texto que consta de varias partes, incluyendo el método (GET), la versión de HTTP (HTTP/1.1) y la URL del sitio web. También se pueden incluir encabezados adicionales, como información sobre el navegador y la conexión.

La función "readString" recibe la respuesta del servidor y la almacena en una variable de tipo String. Esta respuesta es básicamente el código HTML de la página web solicitada. Podemos usar esta respuesta para realizar acciones adicionales en nuestro proyecto de Arduino, como mostrar información en una pantalla LCD o activar un sensor.

##Vea También

- Tutorial de Arduino sobre cómo descargar una página web: https://www.arduino.cc/en/Tutorial/WiFiWebClient
- Biblioteca WiFiClient: https://www.arduino.cc/en/Reference/WiFiClient
- Curso en línea de Arduino para principiantes: https://www.coursera.org/learn/arduino