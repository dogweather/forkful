---
title:                "C: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué

En la programación, enviar solicitudes HTTP es una habilidad esencial para desarrollar aplicaciones web dinámicas y conectarse con servidores externos. Esto permite enviar y recibir datos, tales como formularios en línea o consultas de bases de datos, de manera rápida y eficiente.

## Cómo hacerlo

Primero, necesitamos un URL válido para enviar la solicitud. Luego, usamos la función `http_get()` para enviar una solicitud GET y `http_post()` para una solicitud POST. A continuación, mostramos un ejemplo de cómo enviar una solicitud GET utilizando la biblioteca `curl`.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;
 
  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://ejemplo.com/");
    res = curl_easy_perform(curl);
    /* Manejar el código de respuesta aquí */
    curl_easy_cleanup(curl);
  }
  return 0;
}
```

La salida de este código sería el contenido HTML de la página solicitada.

## Profundizando

Hay muchas opciones que se pueden configurar al enviar una solicitud HTTP, como agregar encabezados específicos o autenticación. También es importante manejar los posibles códigos de respuesta, como errores o redirecciones. Afortunadamente, la biblioteca `curl` tiene una amplia documentación y ejemplos para guiar a los programadores en el manejo de estas situaciones.

## Ver también

- Tutorial de cURL de Codecademy (https://www.codecademy.com/learn/curl)
- Documentación oficial de cURL (https://curl.haxx.se/libcurl/c/)
- Ejemplos de código de cURL en GitHub (https://github.com/curl/curl/tree/master/docs/examples)