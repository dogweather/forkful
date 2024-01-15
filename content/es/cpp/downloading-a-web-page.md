---
title:                "Descargando una página web."
html_title:           "C++: Descargando una página web."
simple_title:         "Descargando una página web."
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## ¿Por qué?

Descargar una página web es útil cuando se desea acceder a su contenido sin necesariamente estar conectado a internet, o cuando se desea guardar una copia para leer en el futuro.

## Cómo hacerlo

Para descargar una página web en C++, primero necesitamos incluir la biblioteca `curl` en nuestro código. Esto nos permitirá realizar solicitudes HTTP y, por lo tanto, descargar el contenido de una página web. Aquí hay un ejemplo de código básico:

```C++
#include <iostream>
#include <curl/curl.h>

int main() {
    // Crear un objeto CURL para realizar solicitudes
    CURL *curl = curl_easy_init();
    if (curl) {
        // Establecer la URL de la página web que deseamos descargar
        curl_easy_setopt(curl, CURLOPT_URL, "https://miwebfavorita.com");
        // Crear un objeto FILE para guardar el contenido de la página web
        FILE *file = fopen("pagina_web.html", "wb");
        // Establecer la opción para escribir el contenido de la página en el archivo
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, file);

        // Realizar solicitud HTTP y guardar el contenido en el archivo
        CURLcode res = curl_easy_perform(curl);

        // Manejar posibles errores
        if (res != CURLE_OK) {
            std::cerr << "Error al descargar la página web: " << curl_easy_strerror(res) << std::endl;
        }

        // Cerrar objetos y liberar memoria
        fclose(file);
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

Al ejecutar este código, se creará un archivo llamado `pagina_web.html`, que contendrá el código HTML de la página web especificada en la URL. Si deseamos guardar el contenido en un `std::string` en lugar de en un archivo, podemos usar `CURLOPT_WRITEFUNCTION` y `CURLOPT_WRITEHEADER` en lugar de `CURLOPT_WRITEDATA`.

## Profundizando

La función `curl_easy_setopt()` nos permite establecer varias opciones para nuestra solicitud HTTP. Por ejemplo, podemos usar `CURLOPT_FOLLOWLOCATION` para seguir redirecciones y descargar el contenido de la página final. También podemos establecer opciones de autenticación con `CURLOPT_USERNAME` y `CURLOPT_PASSWORD` si la página web requiere credenciales.

Para realizar solicitudes más avanzadas, como enviar datos a través de un formulario en una página web, podemos usar `CURLOPT_POST` y `CURLOPT_POSTFIELDS` para enviar datos en el cuerpo de la solicitud.

## Ver también

- Documentación oficial de la biblioteca curl: https://curl.haxx.se/libcurl/c/libcurl.html
- Ejemplos de código de descarga de páginas web en C++: https://curl.haxx.se/libcurl/c/example.html