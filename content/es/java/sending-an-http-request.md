---
title:                "Java: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué enviar una solicitud HTTP?

Enviar una solicitud HTTP es una tarea crucial en la programación Java, ya que permite interactuar con recursos en la web, como datos, imágenes o servicios. Al realizar una solicitud HTTP, el programador puede obtener o enviar información desde una URL específica.

## ¿Cómo hacerlo?

Para enviar una solicitud HTTP en Java, se deben seguir los siguientes pasos:

```Java
// Importar la clase URL y la clase HttpURLConnection
import java.net.URL;
import java.net.HttpURLConnection;

// Crear una URL con la dirección del recurso que deseamos solicitar
URL url = new URL("http://www.ejemplo.com/recurso");

// Abrir una conexión con la URL
HttpURLConnection con = (HttpURLConnection) url.openConnection();

// Establecer el método de solicitud (GET, POST, PUT, DELETE)
con.setRequestMethod("GET");

// Leer la respuesta y mostrarla en la consola
System.out.println(con.getResponseCode() + " " + con.getResponseMessage());
// En este ejemplo, la respuesta será "200 OK"

// Cerrar la conexión
con.disconnect();
```

## Inmersión profunda

Existen diferentes métodos para personalizar una solicitud HTTP en Java. Por ejemplo, se pueden pasar parámetros en una solicitud POST usando el método `setDoOutput()` y escribiendo los datos en la conexión con `getOutputStream()`. También se pueden agregar encabezados personalizados a una solicitud con el método `setRequestProperty()`. Es importante entender el código de respuesta que se recibe al realizar una solicitud HTTP, ya que puede indicar si la operación se realizó con éxito o si hubo algún error.

## Ver también

- Documentación oficial de Java sobre el paquete `java.net`: https://docs.oracle.com/javase/8/docs/api/java/net/package-summary.html
- Tutorial de Java sobre enviar una solicitud HTTP: https://www.baeldung.com/java-http-request