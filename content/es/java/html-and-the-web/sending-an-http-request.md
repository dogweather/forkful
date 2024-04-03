---
date: 2024-01-20 18:00:20.129152-07:00
description: "C\xF3mo hacerlo: Para enviar una petici\xF3n HTTP en Java, el JDK proporciona\
  \ la clase `HttpClient`. Aqu\xED hay un ejemplo de c\xF3mo hacer una petici\xF3\
  n GET a un\u2026"
lastmod: '2024-03-13T22:44:58.934636-06:00'
model: gpt-4-1106-preview
summary: "Para enviar una petici\xF3n HTTP en Java, el JDK proporciona la clase `HttpClient`."
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
Para enviar una petición HTTP en Java, el JDK proporciona la clase `HttpClient`. Aquí hay un ejemplo de cómo hacer una petición GET a un servidor y manejar la respuesta:

```java
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://jsonplaceholder.typicode.com/posts/1"))
                .build();

        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            System.out.println(response.statusCode());
            System.out.println(response.body());
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
```

Output de muestra:

```
200
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum..."
}
```

## Profundización
El envío de peticiones HTTP no es nuevo. Ha sido una piedra angular de la comunicación entre clientes y servidores desde los primeros días de la web. Java ha evolucionado en este aspecto. Antes de `HttpClient` que se introdujo en Java 11, las opciones incluían clases como `HttpURLConnection` y bibliotecas de terceros como Apache HttpClient.

La implementación moderna `HttpClient` soporta HTTP/2 y también hace más fácil manejar respuestas asíncronas, mejorando el rendimiento. Si necesitas trabajar con diferentes métodos HTTP (como POST, PUT, DELETE), `HttpRequest.Builder` ofrece métodos para construir la petición específica que necesitas.

Alternativas como OkHttp y Retrofit siguen siendo populares para ciertos casos de uso y ofrecen su propia gama de características y facilidades.

## Vea También
- Documentación oficial de `HttpClient`: [https://docs.oracle.com/en/java/javase/15/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/15/docs/api/java.net.http/java/net/http/HttpClient.html)
- OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- Retrofit: [https://square.github.io/retrofit/](https://square.github.io/retrofit/)
