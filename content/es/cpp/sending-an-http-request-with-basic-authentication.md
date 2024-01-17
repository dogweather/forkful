---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "C++: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

En programación, el envío de una solicitud HTTP con autenticación básica se refiere a enviar una solicitud de acceso a un servidor web que requiere credenciales de usuario para autorizar el acceso. Los programadores hacen esto para garantizar la seguridad y la privacidad al acceder a cierta información o realizar acciones en un servidor en línea.

## Cómo:

```C++
// Ejemplo de código para enviar una solicitud HTTP con autenticación básica en C++:

#include <iostream>
#include <curl/curl.h>

using namespace std;

int main() {
    // Creamos un objeto CURL para realizar la solicitud
    CURL *curl;
    // Inicializamos la sesión de CURL
    curl = curl_easy_init();

    if (curl) {
        // Establecemos la URL a la que queremos enviar la solicitud
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.ejemplo.com/");
        // Establecemos el método de solicitud a GET
        curl_easy_setopt(curl, CURLOPT_HTTPGET, 1L);
        // Establecemos el nombre de usuario y la contraseña para la autenticación básica
        curl_easy_setopt(curl, CURLOPT_USERPWD, "nombre_usuario:contraseña");
        
        // Realizamos la solicitud y guardamos la respuesta en una variable
        CURLcode res = curl_easy_perform(curl);

        // Si la solicitud fue exitosa, imprimimos la respuesta
        if (res == CURLE_OK) {
            cout << "Respuesta recibida: " << endl;
            cout << curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE) << endl;
        } else {
            // En caso de error, imprimimos el código de error
            cout << "Error al realizar la solicitud: " <<
                curl_easy_strerror(res) << endl;
        }

        // Finalizamos la sesión de CURL
        curl_easy_cleanup(curl);
    }

    return 0;
}
```

### Salida de ejemplo:
```
Respuesta recibida:
200
```

## Profundizando:

- **Contexto histórico:** La autenticación básica fue uno de los primeros métodos utilizados para proteger el acceso a las páginas web en la World Wide Web.
- **Alternativas:** Además de la autenticación básica, existen otros métodos de autenticación más seguros y ampliamente utilizados, como OAuth y JSON Web Tokens (JWT).
- **Detalles de la implementación:** Para enviar una solicitud HTTP con autenticación básica, se deben seguir los siguientes pasos:
    1. Crear un objeto CURL y establecer la URL a la que se desea enviar la solicitud.
    2. Establecer el método de solicitud a GET, POST, PUT, etc.
    3. Establecer el nombre de usuario y contraseña para la autenticación básica con `CURLOPT_USERPWD`.
    4. Realizar la solicitud con `curl_easy_perform`.
    5. Manejar la respuesta recibida, ya sea imprimiéndola o guardándola en una variable.
    6. Finalizar la sesión de CURL con `curl_easy_cleanup`.

## Ver también:

- [Documentación de CURL](https://curl.se/libcurl/)
- [Artículo sobre autenticación básica en MDN Web Docs](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)