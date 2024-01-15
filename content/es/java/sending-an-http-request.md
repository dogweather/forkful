---
title:                "Enviando una solicitud http"
html_title:           "Java: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Enviar una solicitud HTTP es una tarea común en la programación de Java. Esta acción es necesaria cuando se desea comunicar con un servidor de internet, como cuando se envía información de un formulario o se solicita datos de una API.

## Cómo hacerlo

Para enviar una solicitud HTTP en Java, se utiliza la clase HttpUrlConnection y sus métodos correspondientes. Primero, se debe crear una URL válida a la que se desea enviar la solicitud. Luego, se establece la conexión a través de la clase HttpUrlConnection y se configuran los métodos de solicitud apropiados, como GET o POST. Finalmente, se pueden agregar parámetros o información adicional a la solicitud antes de enviarla utilizando los métodos setRequestProperty() y getOutputStream().

A continuación se muestra un ejemplo de cómo enviar una solicitud GET a la URL "https://www.example.com":

```Java
URL url = new URL("https://www.example.com");
HttpUrlConnection con = (HttpUrlConnection) url.openConnection();
con.setRequestMethod("GET");
con.setRequestProperty("User-Agent", "Java/1.8");

int responseCode = con.getResponseCode(); // este método envía la solicitud y devuelve el código de respuesta
System.out.println("Código de respuesta: " + responseCode);

BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream())); // obtener la respuesta del servidor
String inputLine;
StringBuffer content = new StringBuffer();
while ((inputLine = in.readLine()) != null) { // leer la respuesta línea por línea
    content.append(inputLine); // agregar a un StringBuffer
}
in.close(); // cerrar BufferedReader

System.out.println("Respuesta del servidor: " + content.toString()); // imprimir la respuesta
```

La salida de este código será algo similar a lo siguiente:

```
Código de respuesta: 200
Respuesta del servidor: ¡Hola, Mundo!
```

También es importante mencionar que el manejo de excepciones es necesario al realizar una solicitud HTTP, ya que pueden ocurrir errores durante la conexión o la comunicación con el servidor.

## Profundizando en las solicitudes HTTP

Enviar una solicitud HTTP implica una serie de pasos detrás de escena. Primero, el navegador o la aplicación Java formulan la solicitud y la envían a través de la red mediante el protocolo HTTP. Luego, el servidor web recibe la solicitud, la procesa y envía una respuesta de vuelta al cliente con información o contenido solicitado.

En el ejemplo anterior, se utilizó el método GET de la clase HttpUrlConnection para enviar una solicitud sin cuerpo (body) y obtener una respuesta del servidor. Sin embargo, también es posible enviar una solicitud con un cuerpo utilizando el método POST. Esto se utiliza comúnmente al enviar información de un formulario o al crear una nueva entrada en una base de datos.

Además de los métodos GET y POST, también existen otros métodos de solicitud HTTP, como PUT, DELETE y HEAD. Cada uno tiene un propósito específico y se utiliza en diferentes situaciones.

Si deseas profundizar en las solicitudes HTTP en Java, te recomendamos leer la documentación oficial de Oracle sobre la clase HttpUrlConnection y su método correspondiente para cada tipo de solicitud.

## Ver también

- [Documentación oficial de Oracle sobre la clase HttpUrlConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Tutorial de JavaTpoint sobre cómo enviar una solicitud HTTP en Java](https://www.javatpoint.com/httpurlconnection-example)
- [Artículo de Baeldung sobre el manejo de solicitudes HTTP en Java](https://www.baeldung.com/java-http-request)