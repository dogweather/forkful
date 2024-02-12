---
title:                "Descargando una página web"
aliases:
- /es/java/downloading-a-web-page/
date:                  2024-01-20T17:44:31.182277-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Descargar una página web significa obtener su contenido, normalmente en forma de HTML, a través de Internet. Los programadores lo hacen para análisis de datos, monitoreo de cambios, o para alimentar aplicaciones con contenido dinámico.

## Cómo hacerlo:

Para descargar una página web en Java, podemos usar la clase `HttpClient` que facilita realizar peticiones HTTP. Aquí hay un ejemplo simple:

```java
import java.io.IOException;
import java.net.URI;
import java.net.http.*;

public class PaginaWebDownloader {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
            .uri(URI.create("http://ejemplo.com"))
            .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
            .thenApply(HttpResponse::body)
            .thenAccept(System.out::println)
            .join();
    }
}
```

Output de ejemplo (será diferente según el contenido de `http://ejemplo.com`):

```
<!DOCTYPE html>
<html>
<head>
    <title>Ejemplo Título</title>
</head>
<body>
    <p>Este es un ejemplo de contenido de página web.</p>
</body>
</html>
```

## Buceo Profundo

Históricamente, las descargas de páginas web en Java se manejaban con la clase `URLConnection` o bibliotecas de terceros como Apache HttpClient. Desde Java 11, se incluye `HttpClient` que es más moderno y versátil. Al descargar páginas, es importante manejar los códigos de estado HTTP y asegurarse de que no estamos haciendo demasiadas peticiones para evitar ser bloqueados por el servidor. Otras bibliotecas como JSoup o HtmlUnit pueden ser útiles no solo para descargar, sino también para parsear y manejar el HTML de forma más eficiente.

## Ver También

- Documentación oficial de `HttpClient`: https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html
- Tutorial de JSoup para parseo de HTML: https://jsoup.org/cookbook/
- HtmlUnit para pruebas de aplicaciones web: http://htmlunit.sourceforge.net/

Recuerda que estás buscando información actual y té relevante, así que asegúrate de que las fuentes sean recientes y confiables. ¡Buena suerte, y a codificar!
