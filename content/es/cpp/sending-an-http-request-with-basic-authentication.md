---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Enviar una solicitud HTTP con autenticación básica implica la utilización de un protocolo de red simétrica para la autenticación de los datos. Los programadores lo hacen para restringir los recursos sólo a usuarios válidos, garantizando seguridad y privacidad.

## Cómo hacerlo:

Utilizando la librería cURL en C++, puedes enviar una solicitud HTTP con autenticación básica:

```C++
#include <curl/curl.h>

int main() {
    CURL *curl;
    curl = curl_easy_init();
    
    if(curl) {
        curl_easy_setopt(curl, CURLOPT_URL, "https://api.example.com/data");
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_ANY);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "usuario");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "contraseña");
        
        CURLcode res = curl_easy_perform(curl);
        
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() falló: %s\n",
            curl_easy_strerror(res));
        
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

La salida sería simplemente si la autenticación funcionó o no.

## Análisis Profundo:

La autenticación HTTP básica es un método para un cliente HTTP proporcionar un nombre de usuario y una contraseña cuando realiza una solicitud. Inventado en los primeros días de la web, proporciona un mecanismo de seguridad rudimentario para controlar el acceso a los recursos web.

Aunque simple, no es la opción más segura ya que las credenciales se pasan en texto plano. Alternativas más seguras son OAuth y JWT.

La implementación en C++ requiere el uso de librerías como cURL o Boost. En particular, cURL proporciona una API fácil de utilizar para el manejo de credenciales y la transmisión de datos.

## Véase también:

1. [Documentación de cURL](https://curl.se/libcurl/c/)
2. [Autenticación básica de HTTP en Wikipedia](https://es.wikipedia.org/wiki/Autenticación_de_acceso_básico)
3. [Librería Boost.Asio](https://www.boost.org/doc/libs/1_77_0/doc/html/boost_asio.html)
4. [OAuth](https://oauth.net/)
5. [JWT (JSON Web Tokens)](https://jwt.io/)