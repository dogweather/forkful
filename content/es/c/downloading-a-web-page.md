---
title:                "Descargando una página web"
date:                  2024-01-20T17:43:23.928011-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descargando una página web"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

Descargar una página web significa obtener su contenido HTML a través de Internet. Los programadores hacen esto para analizar datos, probar servicios web o alimentar aplicaciones con información actualizada.

## Cómo hacerlo:

Aquí vamos a usar la biblioteca libcurl para descargar una página web simple.

Primero, instala libcurl si aún no lo tienes:
```shell
sudo apt-get install libcurl4-openssl-dev
```

Ahora, el código:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_callback(void *contents, size_t size, size_t nmemb, void *userp) {
    size_t realsize = size * nmemb;
    printf("%.*s", (int)realsize, (char *)contents);
    return realsize;
}

int main(void) {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);
    curl = curl_easy_init();
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);

        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));

        curl_easy_cleanup(curl);
    }
    curl_global_cleanup();

    return 0;
}
```

Compila con:
```shell
gcc -o download download.c -lcurl
```

Ejecútalo y deberías ver el HTML de `http://example.com`.

## Análisis Profundo:

Históricamente, descargar páginas web era más complicado. Se necesitaba manejar sockets y el protocolo HTTP manualmente. Librerías como libcurl han simplificado mucho este proceso.

Alternativas a libcurl son la librería `socket` para C, si quieres más control y no te importa escribir más código, y otras librerías de alto nivel como `Qt Network` si estás desarrollando aplicaciones con Qt.

La implementación con libcurl es directa. Configuras opciones para tu `CURL` handle, como la URL y la función de callback que maneja los datos recibidos (en este ejemplo, simplemente imprimimos los datos al standard output), y luego ejecutas la petición con `curl_easy_perform`.

## Ver También:

- Documentación de libcurl: https://curl.se/libcurl/c/
- Guía de programación de socket en C: https://beej.us/guide/bgnet/
- Qt Network: https://doc.qt.io/qt-5/qtnetwork-index.html
