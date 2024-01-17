---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "C: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué enviar una solicitud HTTP con autenticación básica?

Enviar una solicitud HTTP con autenticación básica es una forma de establecer un nivel básico de seguridad al comunicarse con un servidor en línea. Los programadores utilizan este método para asegurarse de que solo los usuarios autorizados puedan acceder a ciertos recursos en línea.

## Cómo:

Aquí hay un ejemplo de cómo enviar una solicitud HTTP con autenticación básica en C, utilizando la biblioteca libcurl:

```C
#include <curl/curl.h>

int main(void)
{
    CURL *curl;
    CURLcode res;

    curl = curl_easy_init();
    if(curl) {
        /* Establecer URL a la que se enviará la solicitud */
        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com");

        /* Establecer el nombre de usuario y la contraseña para la autenticación básica */
        curl_easy_setopt(curl, CURLOPT_USERPWD, "username:password");

        /* Realizar la solicitud HTTP */
        res = curl_easy_perform(curl);

        /* Verificar el resultado de la solicitud */
        if(res != CURLE_OK)
            fprintf(stderr, "curl_easy_perform() falló: %s\n",
                    curl_easy_strerror(res));

        /* ¡No olvides limpiar! */
        curl_easy_cleanup(curl);
    }
    return 0;
}
```

Este código establece la URL a la que se desea enviar la solicitud, así como el nombre de usuario y la contraseña para la autenticación básica. A continuación, se realiza la solicitud HTTP utilizando la función `curl_easy_perform` y se verifica el resultado. Finalmente, se limpia y se cierra la conexión.

## Inmersión profunda: 

La autenticación básica es una forma muy simple de autenticación en la que el cliente envía un nombre de usuario y contraseña codificados en texto plano con cada solicitud HTTP. Sin embargo, este método tiene sus limitaciones, ya que la información de autenticación se puede interceptar y comprometer fácilmente.

Como alternativa a la autenticación básica, existen otros métodos de autenticación más seguros, como OAuth y OpenID. Además, también es posible implementar autenticación básica utilizando SSL para encriptar la información de autenticación y hacerla más segura.

En términos de implementación, es importante tener en cuenta que algunos servidores pueden exigir una configuración específica para utilizar la autenticación básica a través de HTTP. Por lo tanto, es recomendable consultar la documentación del servidor en particular antes de utilizar este método en tu código.

## Ver también:

- [Documentación de libcurl](https://curl.haxx.se/libcurl/)
- [Autenticación básica en HTTP](https://www.rfc-editor.org/rfc/rfc7617.txt)
- [AuthBasic en la wiki de Mozilla](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication#Autenticación_básica)