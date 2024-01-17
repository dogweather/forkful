---
title:                "Descargando una página web"
html_title:           "C++: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Descargar una página web es el proceso de obtener el código HTML de una página web y guardarla en tu dispositivo. Los programadores suelen hacer esto para analizar la estructura y el contenido de una página web o para automatizar tareas como la extracción de datos.

## Cómo:
### Ejemplo 1:
```C++
#include <iostream>
#include <curl/curl.h>

int main() {
  CURL *curl;
  CURLcode res;

  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.ejemplo.com/");
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }
  return 0;
}
```
#### Salida:
```
<!DOCTYPE html>
<html>
<head>
  <title>Ejemplo</title>
</head>
<body>
  <h1>Bienvenidos a Ejemplo</h1>
  <p>Esta es una página de ejemplo</p>
</body>
</html>
```

### Ejemplo 2:
```C++
#include <iostream>
#include <fstream>
#include <curl/curl.h>

static size_t WriteCallback(void *data, size_t size, size_t nmemb, void *userp) {
  ((std::string*)userp)->append((char*)data, size * nmemb);
  return size * nmemb;
}

int main() {
  CURL *curl;
  CURLcode res;

  std::string htmlString;
  curl = curl_easy_init();
  if (curl) {
    curl_easy_setopt(curl, CURLOPT_URL, "https://www.ejemplo.com/");
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &htmlString);
    res = curl_easy_perform(curl);
    curl_easy_cleanup(curl);
  }

  std::ofstream output("html.txt");
  if (output.is_open()) {
    output << htmlString;
    output.close();
  } else {
    std::cout << "Error al guardar archivo\n";
  }

  return 0;
}
```
#### Salida:
El código HTML de la página se guarda en un archivo llamado "html.txt".

## Profundizando:
El proceso de descarga de páginas web ha cambiado mucho a lo largo de los años. Antes, los programadores lo hacían utilizando sockets y construyendo manualmente las solicitudes y respuestas HTTP. Pero ahora, gracias a librerías como cURL en C++, es mucho más sencillo y eficiente.

Existen otras alternativas a cURL, como usar librerías específicas para descarga de páginas web, pero cURL sigue siendo una de las opciones más populares y robustas.

Para descargar una página web, cURL utiliza el protocolo HTTP para establecer una conexión con el servidor y solicitar el código HTML de la página. Luego, el código devuelto por el servidor es recibido y procesado por cURL.

## Ver también:
- [Documentación de cURL](https://curl.haxx.se/docs/)
- [Tutorial de descarga de páginas web en C++ con cURL](https://curl.haxx.se/libcurl/c/example.html)