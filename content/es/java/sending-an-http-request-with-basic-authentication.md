---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Java: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP con autenticación básica es una forma segura y sencilla de proteger la comunicación entre un cliente y un servidor. Esto permite que solo los usuarios autorizados puedan acceder a ciertos recursos en un sitio web o aplicación.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en Java, primero debemos crear un objeto `URL` con la dirección del servidor y el recurso al que queremos acceder. Luego, se debe crear un objeto `HttpURLConnection` a partir de la URL y especificar el tipo de solicitud que se desea realizar usando el método `setRequestMethod()`. A continuación, se debe establecer el encabezado `Authorization` con las credenciales de autenticación básica en formato Base64 utilizando el método `setRequestProperty()`. Finalmente, se pueden agregar parámetros adicionales a la solicitud y se puede obtener la respuesta del servidor utilizando los métodos `getInputStream()` y `getResponseCode()`.

Un ejemplo de código completo se muestra a continuación:

```java
URL url = new URL("https://www.mi-servidor.com/recurso");
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET"); // Se puede cambiar a POST si se desea
String encodedCredentials = Base64.getEncoder().encodeToString("usuario:contraseña".getBytes());
con.setRequestProperty("Authorization", "Basic " + encodedCredentials);

// Se pueden agregar parámetros adicionales con el método setRequestProperty()

int responseCode = con.getResponseCode();
InputStream inputStream = con.getInputStream();
// Lógica para manejar la respuesta del servidor...
```

El resultado de la solicitud dependerá de la respuesta del servidor y de los parámetros y encabezados específicos de la solicitud.

## Profundizando

La autenticación básica funciona mediante el envío de las credenciales de usuario en texto plano a través del encabezado `Authorization`. Sin embargo, esto no es suficiente para mantener una comunicación completamente segura, ya que las credenciales pueden ser interceptadas por un atacante. Por lo tanto, es importante usar HTTPS en lugar de HTTP para establecer una conexión cifrada entre el cliente y el servidor.

Otra forma de mejorar la seguridad es utilizando un mecanismo de autenticación más robusto, como la autenticación de tokens o con SSL.

## Ver también

- [Documentación de Java para HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html) 
- [Tutorial sobre autenticación básica en Java](https://www.baeldung.com/java-http-request)