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

## ¿Qué y por qué?

Enviar una solicitud HTTP es una acción común en la programación. Es el proceso de enviar una petición desde un cliente a un servidor, con el objetivo de obtener o enviar información. Los programadores hacen esto para interactuar con aplicaciones web, acceder a datos de una API o para realizar acciones en un servidor.

## Cómo hacerlo:

Java proporciona una clase llamada HttpURLConnection para enviar solicitudes HTTP. Primero, debes importar la clase con la palabra clave "import":

```Java
import java.net.HttpURLConnection;
```

Luego, puedes usar esta clase para crear una conexión al servidor y enviar una solicitud. Por ejemplo, si queremos obtener datos de la API de GitHub, podemos hacer lo siguiente:

```Java
// Crear la URL de la API
URL url = new URL("https://api.github.com/users/martinezpablo");

// Abrir una conexión con la URL
HttpURLConnection con = (HttpURLConnection) url.openConnection();

// Establecer el método de la solicitud
con.setRequestMethod("GET");

// Obtener la respuesta del servidor
int responseCode = con.getResponseCode();

// Imprimir la respuesta
System.out.println("Response code: " + responseCode);
```

La salida de este código sería "Response code: 200", lo que significa que hemos recibido una respuesta exitosa del servidor. Podemos continuar manipulando la conexión para obtener los datos deseados.

## Inmersión profunda:

La necesidad de enviar solicitudes HTTP surge de la necesidad de interacción entre aplicaciones web. Antes de HTTP, se utilizaba el protocolo FTP, que solo permitía la transferencia de archivos. Con el aumento de la web, se necesitaba un protocolo más versátil para permitir la comunicación entre las aplicaciones.

Java también proporciona otras clases para enviar solicitudes HTTP, como HttpClient y URLConnection. HttpClient ofrece una API más simple y fácil de usar, mientras que URLConnection permite una mayor personalización en la solicitud.

## Ver también:

- [Documentación oficial de Java sobre HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Comparación entre HttpClient y URLConnection](https://stackoverflow.com/questions/2793150/httpclient-vs-httpurlconnection)