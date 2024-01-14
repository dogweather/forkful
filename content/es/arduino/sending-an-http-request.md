---
title:                "Arduino: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

##Por qué

Enviar solicitudes HTTP es una habilidad útil para los programadores de Arduino, ya que les permite comunicarse con servidores externos y acceder a recursos en línea. Esto es especialmente útil para proyectos que requieren conectividad a internet, como aplicaciones de domótica o monitoreo remoto.

##Cómo hacerlo

Para enviar una solicitud HTTP en Arduino, primero debemos importar la librería necesaria mediante la línea de código:

```Arduino
#include <ArduinoHttpClient.h>
```

A continuación, creamos un objeto cliente HTTP y especificamos el servidor y el puerto al que queremos enviar la solicitud:

```Arduino
HttpClient client = HttpClient(wifi, "www.ejemplo.com", 80);
```

Luego, definimos el tipo de solicitud que queremos enviar (GET, POST, PUT, DELETE) y la ruta del recurso en el servidor:

```Arduino
client.get("/recurso");
```

Finalmente, ejecutamos la solicitud y almacenamos la respuesta en una variable:

```Arduino
int status = client.responseStatusCode();
```

Si la solicitud es exitosa, la variable `status` contendrá el código de estado de la respuesta (200 para una solicitud exitosa), de lo contrario, el código reflejará el tipo de error.

##Profundizando

Hay muchos más detalles que podemos explorar al enviar una solicitud HTTP en Arduino. Podemos añadir encabezados a nuestra solicitud para enviar información adicional, como autorización o datos de contenido. También podemos enviar parámetros en la solicitud para personalizarla aún más.

Además, podemos utilizar la función de respuesta del cliente para obtener más información sobre la respuesta recibida del servidor, como el contenido del cuerpo o los encabezados de la respuesta.

Otra consideración importante es la seguridad al enviar solicitudes HTTP desde Arduino. Es recomendable utilizar HTTPS en lugar de HTTP para cifrar los datos enviados y protegerlos de posibles interceptaciones.

##Ver también

Aquí hay algunos recursos adicionales que pueden ser útiles para aprender más sobre el envío de solicitudes HTTP en Arduino:

- [Documentación oficial de ArduinoHttpClient](https://www.arduino.cc/en/Reference/ArduinoHttpClient)
- [Tutorial de Envío de Solicitudes HTTP con Arduino](https://maker.pro/arduino/tutorial/how-to-send-an-http-request-from-arduino)
- [Ejemplo de Envío de Parámetros en una Solicitud HTTP](https://randomnerdtutorials.com/esp32-http-post-arduino/)
- [Comparación entre HTTP y HTTPS](https://www.cloudflare.com/en-au/learning/ssl/why-is-http-not-secure/)