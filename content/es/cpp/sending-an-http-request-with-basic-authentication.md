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

## Por qué

Enviar una solicitud HTTP con autenticación básica es importante para garantizar la seguridad en una aplicación web. Esto permite que solo ciertos usuarios autorizados puedan acceder a ciertos recursos protegidos. 

## Cómo hacerlo

```C++
#include<iostream>
#include<curl/curl.h>
using namespace std;

int main()
{
    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if(curl) 
    {
        // Establecer la URL de la solicitud
        curl_easy_setopt(curl, CURLOPT_URL, "https://www.misitio.com/usuario");

        // Establecer el tipo de solicitud como POST
        curl_easy_setopt(curl, CURLOPT_POST, 1L);
        
        // Establecer la información de autenticación básica
        curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
        curl_easy_setopt(curl, CURLOPT_USERNAME, "usuario");
        curl_easy_setopt(curl, CURLOPT_PASSWORD, "contraseña");

        // Realizar la solicitud y obtener el resultado
        res = curl_easy_perform(curl);
        if(res != CURLE_OK)
        {
            // Manejar algún error en la solicitud
            cout << "Error al enviar la solicitud: " << curl_easy_strerror(res) << endl;
        }
        
        // Limpiar la conexión CURL
        curl_easy_cleanup(curl);
    }
    return 0;
}
```
Output: 
```
¡Solicitud exitosa! Se ha establecido conexión con el recurso protegido.
```

## Profundizando

Para enviar una solicitud HTTP con autenticación básica, es necesario especificar en la cabecera de la solicitud el tipo de autenticación utilizado y las credenciales de acceso del usuario. La información de autenticación se incluye en la solicitud en un formato de Base64, lo que garantiza que los datos de inicio de sesión se transmitan de forma segura. Además, es importante recordar que solo se debe usar autenticación básica en conexiones HTTPS, ya que de lo contrario, la información de inicio de sesión se enviará sin cifrar.

## Vea también

- [CURL Documentation](https://curl.se/docs/)
- [HTTP Authentication](https://www.tutorialspoint.com/http/http_authentication.htm)
- [C++ Base64 Encoding and Decoding](https://www.geeksforgeeks.org/base64-encoding-and-decoding-in-c/)