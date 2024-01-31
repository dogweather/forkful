---
title:                "Enviando una solicitud http con autenticación básica"
date:                  2024-01-20T18:01:09.997226-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando una solicitud http con autenticación básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Enviar una solicitud HTTP con autenticación básica significa incluir credenciales de usuario (usuario y contraseña) codificadas en base64 en la cabecera HTTP para acceder a recursos restringidos. Los programadores lo hacen para asegurar que el acceso a ciertas partes de una aplicación web solo sea posible para usuarios autorizados.

## Cómo Hacerlo:

En C, la biblioteca libcurl nos facilita el proceso:

```C
#include <stdio.h>
#include <curl/curl.h>

int main() {
    CURL *curl;
    CURLcode res;

    curl_global_init(CURL_GLOBAL_ALL);

    curl = curl_easy_init();
    if(curl) {
        // Estableciendo la URL a la que enviar la solicitud.
        curl_easy_setopt(curl, CURLOPT_URL, "http://ejemplo.com/recurso");

        // Configurando la autenticación básica.
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, (long)CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERPWD, "usuario:contraseña");

        // Realizando la solicitud.
        res = curl_easy_perform(curl);
        
        // Comprobando errores.
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() falló: %s\n", curl_easy_strerror(res));

        // Limpieza.
        curl_easy_cleanup(curl);
    }

    curl_global_cleanup();

    return 0;
}
```

Output típico (dependiendo del recurso solicitado, puede variar):

```
{"mensaje": "Acceso concedido a su recurso solicitado."}
```

## Profundizando

Históricamente, la autenticación básica ha sido un método simple pero no muy seguro para controlar el acceso, principalmente por enviar las credenciales en texto claro codificado, no encriptado. Alternativas modernas más seguras incluyen tokens JWT, OAuth, y autenticación de dos factores.

La implementación de la autenticación básica con libcurl es directa: configuramos la cabecera `Authorization` automáticamente al proporcionarle el usuario y contraseña a `CURLOPT_USERPWD`. La biblioteca se encarga de la codificación en base64 por nosotros. Aunque sea simple, usar HTTPS es esencial para proteger la información contra interceptaciones.

## Ver También

- Documentación oficial de libcurl: https://curl.se/libcurl/c/
- RFC 7617, "The 'Basic' HTTP Authentication Scheme": https://tools.ietf.org/html/rfc7617
- Tutorial de libcurl para C: https://curl.se/libcurl/c/libcurl-tutorial.html
- Comparación de métodos de autenticación: https://www.owasp.org/index.php/Authentication_Cheat_Sheet
