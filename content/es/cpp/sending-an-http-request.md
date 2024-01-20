---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Un 'HTTP Request' es una solicitud que tu programa envía a un servidor web. Los programadores lo utilizan para interactuar con APIs de terceros, descargar contenido de internet, enviar datos de formulario, entre otras cosas.

## Cómo hacer:

Para enviar una solicitud HTTP en C++, puedes usar la biblioteca cURL, así:

```C++
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_DEFAULT);

    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        res = curl_easy_perform(curl);

        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() falló: %s\n", curl_easy_strerror(res));
        }

        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```
Este código solicita el contenido del sitio "http://example.com". Si la solicitud se realiza con éxito, el contenido se imprimirá en la salida estándar.

## En profundidad:

Las solicitudes HTTP datan de la creación del protocolo HTTP en 1991. En C++, cURL ha sido la biblioteca estándar para manejar solicitudes HTTP desde su lanzamiento en 1997.

Se puede usar otras bibliotecas como Boost.Asio or POCO para enviar solicitudes HTTP en C++, pero cURL es la más común debido a su simplicidad y amplio soporte.

Cuando envías una solicitud HTTP usando cURL, en realidad estás creando una conexión TCP con el servidor, enviando los datos de la solicitud y esperando la respuesta. Los detalles de este proceso son manejados por la biblioteca cURL, pero es útil entender lo que ocurre a bajo nivel.

## Ver también:

- Documentación de cURL: https://curl.haxx.se/libcurl/c/
- Tutorial de Boost.Asio: https://www.boost.org/doc/libs/1_70_0/doc/html/boost_asio/tutorial.html
- Tutorial de POCO HTTP: https://pocoproject.org/docs/00200-HTTPUserGuide.html