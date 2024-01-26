---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:01:17.534269-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Enviar una solicitud HTTP con autenticación básica significa incluir credenciales de usuario y contraseña codificadas en base64 en el encabezado de la solicitud. Los programadores lo hacen para acceder a recursos protegidos en un servidor web de manera simple y directa.

## How to:
Para enviar una solicitud HTTP con autenticación básica en C++, necesitarás alguna biblioteca como `Curl` o `Boost.Beast`. Aquí hay un ejemplo usando `Curl`:

```C++
#include <iostream>
#include <string>
#include <curl/curl.h>

int main() {
    CURL *curl = curl_easy_init();
    if(curl) {
        std::string userPwd = "usuario:contraseña"; // Reemplaza con tus credenciales
        curl_easy_setopt(curl, CURLOPT_URL, "http://tu-sitio.com/api");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, userPwd.c_str());
        
        CURLcode res = curl_easy_perform(curl);
        if(res != CURLE_OK) {
            std::cerr << "Error en curl_easy_perform(): " << curl_easy_strerror(res) << std::endl;
        }
        
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

La salida será dependiente del servidor y lo que responda a la solicitud con las credenciales proporcionadas.

## Deep Dive
La autenticación básica HTTP es un método que ha estado en uso desde los primeros días de la web. Envía el nombre de usuario y la contraseña en texto claro, codificados en base64, lo que no es seguro sobre conexiones no cifradas. Es recomendable usar HTTPS al implementar autenticación básica para evitar la exposición de credenciales.

Alternativas a la autenticación básica incluyen OAuth, tokens JWT y claves API, cada una con sus propios contextos de uso y niveles de seguridad. 

Implementar este tipo de autenticación en C++ con bibliotecas como `Curl` facilita el proceso, ya que estas bibliotecas gestionan los detalles del protocolo HTTP y la codificación en base64.

## See Also
- Documentación de libcurl: https://curl.haxx.se/libcurl/c/
- Documentación de Boost.Beast: https://www.boost.org/doc/libs/release/libs/beast/
- Wikipedia HTTP Basic Authentication: https://es.wikipedia.org/wiki/Autenticación_básica_en_HTTP
- Understanding HTTP Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
