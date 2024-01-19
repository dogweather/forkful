---
title:                "Descargando una página web"
html_title:           "Arduino: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Descargar una página web es un proceso donde un programa recupera el contenido de una URL específica y lo almacena localmente. Los programadores realizan esto para analizar la estructura de la página web, revisar su contenido, realizar pruebas de rendimiento, entre otras tareas.

## Cómo hacerlo:

Las siguientes líneas de código muestran cómo puedes descargar una página web utilizando la librería libcurl de C.

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode res;

  curl_global_init(CURL_GLOBAL_DEFAULT);

  curl = curl_easy_init();
  if(curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.ejemplo.com");

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() error: %s\n",
              curl_easy_strerror(res));

    curl_easy_cleanup(curl);
  }

  curl_global_cleanup();

  return 0;
}
```
Si todo va bien, este programa descargará la página https://www.ejemplo.com y la enviará a la salida estándar.

## Profundizando

Primero, necesitamos agregar un poco de contexto histórico sobre la necesidad de descargar páginas web. Desde la invención de la web, los programadores han necesitado trabajar con datos online. Hemos venido utilizando diferentes técnicas - desde el básico cliente de telnet hasta el elegante libcurl de hoy.

Hay varias maneras de descargar una página web en C, `libcurl` siendo solo una de ellas. `wget` es un programa de línea de comandos que puedes usar para descargar una página web o `WinInet` si estás programando en Windows.

En realidad, cuando descargamos una página web, estamos haciendo una petición GET a un servidor web. Este responde con el HTML de la página, que nuestros programas pueden interactuar. Saber estos detalles te ayudará a entender y solucionar problemas que puedes enfrentar durante el proceso.

## Ver también

Aquí están algunos enlaces útiles para que puedas explorar más a fondo:

- [Get a file using the HTTP protocol - libcurl](https://curl.se/libcurl/c/http3.html)
- [Windows HTTP Services (WinHTTP)](https://docs.microsoft.com/en-us/windows/win32/winhttp/about-winhttp)
- [GNU Wget Manual](https://www.gnu.org/software/wget/manual/wget.html)