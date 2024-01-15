---
title:                "Descargar una página web"
html_title:           "C: Descargar una página web"
simple_title:         "Descargar una página web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

<br>

## ¿Por qué descargar una página web?

Descargar una página web puede ser útil por diversas razones, como guardar una copia de seguridad, acceder a contenido sin conexión a internet o analizar su estructura para propósitos educativos o profesionales.

<br>

## Cómo hacerlo

Para descargar una página web utilizando C, podemos utilizar la librería `libcurl`. Primero, debemos incluir la librería en nuestro código:

```C
#include <stdio.h>
#include <curl/curl.h>
```

Luego, necesitamos definir una función de callback que se encargará de guardar los datos recibidos en un archivo:

```C
size_t write_callback(char *ptr, size_t size, size_t nmemb, void *userdata) {
   FILE *fp = (FILE *)userdata;
   return fwrite(ptr, size, nmemb, fp);
}
```

A continuación, utilizamos la función `curl_easy_init()` para inicializar una sesión de `libcurl` y la función `curl_easy_setopt()` para establecer las opciones de nuestra solicitud, como la URL y la función de callback:

```C
CURL *curl;
CURLcode result;
curl = curl_easy_init();

if(curl) {
  FILE *fp = fopen("pagina.html", "wb");
  curl_easy_setopt(curl, CURLOPT_URL, "https://ejemplo.com/");
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
```

Por último, ejecutamos la solicitud utilizando la función `curl_easy_perform()` y cerramos la sesión con `curl_easy_cleanup()`:

```C
  result = curl_easy_perform(curl);
  curl_easy_cleanup(curl);
  fclose(fp);
}
```

Al ejecutar nuestro código, se descargará la página web en el archivo `pagina.html` en nuestro directorio actual.

<br>

## Detalles técnicos

`libcurl` es una librería de código abierto que nos permite realizar solicitudes HTTP en múltiples plataformas. Utilizando la función `curl_easy_setopt()`, podemos establecer diferentes opciones para personalizar nuestras solicitudes, como el tipo de petición, encabezados y autenticación. Para más información, podemos consultar la documentación oficial de `libcurl` o revisar el código fuente de la librería.

<br>

## Ver también

- [Documentación de libcurl](https://curl.se/libcurl/)
- [Código fuente de libcurl](https://github.com/curl/curl)
- [Ejemplo de descarga de una página web en C](https://www.geeksforgeeks.org/c-program-download-webpages-program/)