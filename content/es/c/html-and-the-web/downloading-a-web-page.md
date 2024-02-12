---
title:                "Descargando una página web"
aliases: - /es/c/downloading-a-web-page.md
date:                  2024-02-03T17:55:36.191596-07:00
model:                 gpt-4-0125-preview
simple_title:         "Descargando una página web"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Descargar una página web en C implica acceder programáticamente al contenido de una página web a través de internet y guardarla localmente para su procesamiento o uso sin conexión. Los programadores a menudo se involucran en esto para consumir servicios web, extraer contenido web o interactuar directamente con recursos en línea desde sus aplicaciones.

## Cómo hacerlo:

Para descargar una página web en C, un enfoque popular es usar la biblioteca libcurl, una biblioteca de transferencia de URL del lado del cliente eficiente y portátil. Asegúrate de tener libcurl instalado y enlazado en tu proyecto. Aquí hay un ejemplo que demuestra cómo usar libcurl para descargar el contenido de una página web:

```c
#include <stdio.h>
#include <curl/curl.h>

size_t write_data(void *ptr, size_t size, size_t nmemb, FILE *stream) {
    size_t written = fwrite(ptr, size, nmemb, stream);
    return written;
}

int main(void) {
    CURL *curl;
    FILE *fp;
    CURLcode res;
    char *url = "http://example.com";
    char outfilename[FILENAME_MAX] = "./pagina_descargada.html";

    curl = curl_easy_init(); // Inicializar una sesión fácil de libcurl
    if (curl) {
        fp = fopen(outfilename,"wb");
        curl_easy_setopt(curl, CURLOPT_URL, url);
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data); // Callback para escribir los datos recibidos
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp); // Establecer el puntero del archivo para escribir los datos

        res = curl_easy_perform(curl); // Realizar la descarga del archivo
        if(res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() falló: %s\n",
                    curl_easy_strerror(res));
        }

        /* siempre limpiar */
        curl_easy_cleanup(curl); // Limpiar la sesión fácil
        fclose(fp); // Cerrar el flujo de archivo
    }
    return 0;
}
```
Salida de muestra (sin salida visible en la consola): Este código descarga el contenido en la URL especificada y lo guarda en un archivo llamado `pagina_descargada.html`. Revisa el directorio de tu programa para ver este archivo y el contenido descargado.

## Inmersión Profunda:

Históricamente, descargar contenido web en C era más engorroso, requiriendo programación manual de sockets y manejo del protocolo HTTP. Libcurl abstrae estas complejidades, ofreciendo una API robusta y de alto nivel para transferencia de datos a través de la web.

Aunque libcurl simplifica las solicitudes HTTP en C, lenguajes de programación modernos como Python con su biblioteca `requests` o JavaScript (Node.js) con varias bibliotecas de clientes HTTP pueden ofrecer una sintaxis más intuitiva y soporte integrado para JSON y otros formatos de datos comúnmente usados en la comunicación web. Sin embargo, C y libcurl proporcionan una solución de alto rendimiento y estable para sistemas donde la eficiencia, el control detallado o la integración en bases de código C existentes son críticos. También vale la pena mencionar que C, combinado con libcurl, puede usarse para más que solo descargar páginas web; es capaz de manejar FTP, SMTP y mucho más, convirtiéndolo en una herramienta versátil en el kit de herramientas de un programador.
