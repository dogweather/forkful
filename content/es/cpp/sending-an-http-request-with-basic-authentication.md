---
title:                "C++: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

En la programación de C++, es común realizar solicitudes HTTP a través de una red para obtener datos de un servidor externo. Para asegurar la autenticación y proteger los datos, es importante aprender a enviar solicitudes HTTP con autenticación básica.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en C++, se pueden seguir los siguientes pasos:

1. Importar las librerías necesarias en el archivo:

```C++
#include <iostream>
#include <curl/curl.h>
```

2. Definir una estructura para almacenar las credenciales de autenticación:

```C++
struct auth_credentials {
    const char* username;
    const char* password;
};
```

3. Crear una función para configurar la solicitud HTTP con autenticación básica. Esta función recibirá la URL y las credenciales de autenticación como parámetros:

```C++
CURLcode send_request(std::string url, struct auth_credentials credentials) {

    CURL *curl;
    CURLcode res;
    curl = curl_easy_init();
    if (curl) {
        // Configuración de la URL
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        // Configuración de la autenticación básica
        curl_easy_setopt(curl, CURLOPT_USERNAME, credentials.username);
        curl_easy_setopt(curl, CURLOPT_PASSWORD, credentials.password);
        // Envío de la solicitud
        res = curl_easy_perform(curl);
        // Cierre de la conexión
        curl_easy_cleanup(curl);
        // Retorno del resultado
        return res;
    }
}
```

4. Llamar a la función con la URL y las credenciales deseadas:

```C++
struct auth_credentials credentials;
credentials.username = "usuario";
credentials.password = "contraseña";
send_request("www.ejemplo.com", credentials);
```

5. Analizar la respuesta recibida en caso de éxito o error:

```C++
if (res == CURLE_OK) {
    std::cout << "Solicitud exitosa!" << std::endl;
} else {
    std::cout << "Error en la solicitud: "<< curl_easy_strerror(res) << std::endl;
}
```

## Profundizando

La autenticación básica en HTTP se basa en el envío de credenciales de usuario y contraseña en cada solicitud realizada al servidor. Estas credenciales son codificadas en base64 para protegerlas durante la transmisión. Sin embargo, este método de autenticación puede ser vulnerable a ataques de interceptación y debe ser utilizado en conjunto con otras medidas de seguridad.

## Vea también

- [Documentación de la librería libcurl](https://curl.se/libcurl/)
- [Tutorial sobre autenticación básica en HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Authentication)