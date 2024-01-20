---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Envío de solicitudes HTTP en Java

## ¿Qué & Por Qué?
El envío de una solicitud HTTP es básicamente solicitar o enviar datos a un servidor a través del protocolo HTTP. Los programadores lo hacen para interactuar con servicios web, consumir APIs y, en general, facilitar la comunicación entre los sistemas.

## ¿Cómo se hace?

Aquí hay un ejemplo simple de cómo enviar una solicitud HTTP GET usando HttpClient en Java.

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {

   public static void main(String[] args) throws Exception {

       HttpClient client = HttpClient.newHttpClient();
       HttpRequest request = HttpRequest.newBuilder()
               .uri(URI.create("http://example.com"))
               .build();

       HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

       System.out.println(response.body());
   }
}
```
Ejecutar este programa imprimirá el contenido de la página web example.com.

## Un vistazo más profundo

### Contexto Histórico
HttpClient fue introducido en Java 11 como una mejora respecto a las alternativas más antiguas, como `java.net.URL` y `java.net.HttpURLConnection`, cuyas funcionalidades eran limitadas y más difíciles de usar.

### Alternativas
- Apache HttpClient: Aunque requiere dependencias adicionales, es una alternativa poderosa con más funcionalidades.
- OkHttp: Es una opción eficiente que incluye un cliente HTTP/2.

### Detalles de implementación
En el ejemplo anterior, `HttpResponse.BodyHandlers.ofString()` se usa para convertir la respuesta en una cadena. Sin embargo, hay otros manipuladores disponibles para otras necesidades, como manejar la respuesta como un archivo.

## Ver También
- Documentación oficial de Java HttpClient: [Aquí](https://docs.oracle.com/en/java/javase/13/docs/api/java.net.http/java/net/http/HttpClient.html)
- Tutorial de HttpClient en Java: [Aquí](https://www.baeldung.com/java-11-http-client)
- Comparación de librerías HTTP en Java: [Aquí](https://www.baeldung.com/httpclient-vs-httpurlconnection-vs-okhttp)