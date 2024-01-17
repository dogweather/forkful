---
title:                "Descargando una página web"
html_title:           "C: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es un proceso en el que el código de una página (HTML, CSS, Javascript, etc.) se recibe y se guarda en el ordenador para poder ser visualizado por el usuario. Los programadores lo hacen para poder analizar el código, realizar cambios o utilizarlo en sus propios programas.

## Cómo:

Para descargar una página web en C, se puede utilizar la librería `curl`. Aquí hay un ejemplo básico de cómo descargar una página y guardarla en un archivo:

```
#include <stdio.h>
#include <curl/curl.h>

// Función para guardar el contenido de la página en un archivo.
// Se llama cada vez que se recibe un dato.
static size_t write_data(void *ptr, size_t size, size_t nmemb, void *stream)
{
  size_t written = fwrite(ptr, size, nmemb, (FILE *)stream);
  return written;
}

int main()
{
  // Inicializar el objeto curl.
  CURL *curl = curl_easy_init();

  if(curl) {
    // Establecer la URL a descargar.
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.example.com");

    // Establecer la función para guardar los datos.
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_data);

    // Indicar el archivo en el que se guardarán los datos.
    FILE *file = fopen("pagina.html", "w+");
    if(file) {
      curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);

      // Realizar la descarga.
      CURLcode result = curl_easy_perform(curl);

      // Comprobar si hubo algún error.
      if(result != CURLE_OK)
      {
        printf("Error al descargar la página: %s\n",
               curl_easy_strerror(result));
      }

      // Cerrar el archivo.
      fclose(file);
    }

    // Limpiar el objeto curl.
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

Si se ejecuta este programa, se descargará la página `https://www.example.com` y se guardará en un archivo llamado `pagina.html`.

## Profundizando:

La librería `curl` se encuentra ampliamente disponible en diferentes sistemas operativos y es una buena opción para descargar páginas web en C. Sin embargo, también existen otras alternativas como `libmicrohttpd` o `libcurlpp` que ofrecen una interfaz más fácil de utilizar con C++. Además, es importante tener en cuenta que descargar páginas web puede ser un proceso algo complejo debido a cuestiones como autenticación o manejo de cookies.

## Ver también:

- Documentación oficial de `libcurl`: https://curl.haxx.se/libcurl/
- Tutorial de descarga de páginas web con C: https://www.mkssoftware.com/docs/man3/curl_easy_setopt.3.asp
- Otras alternativas para descargar páginas web en C: https://www.slant.co/topics/3387/~libraries-for-downloading-any-online-content-with-c