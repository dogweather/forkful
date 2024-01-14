---
title:                "C++: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por qué enviar una solicitud HTTP?

En la programación moderna, enviar solicitudes HTTP es una parte esencial para integrar aplicaciones y servicios en línea. Permite la transferencia de datos entre diferentes sistemas y es especialmente útil para conectarse con APIs externas o para realizar peticiones a servidores remotos.

## Cómo hacerlo

Para enviar una solicitud HTTP en C++, podemos utilizar la librería `libcurl`, que es una herramienta de transferencia de datos en redes. Primero, debemos incluir la librería en nuestro código:

```C++
#include <curl/curl.h>
```

Luego, creamos una variable `CURL` y una `CURLcode` para almacenar información sobre la solicitud y su respuesta.

```C++
CURL *curl;
CURLcode res;
```

A continuación, inicializamos `libcurl` con la función `curl_global_init()` y creamos una variable para almacenar la URL del servidor al que deseamos enviar nuestra solicitud.

```C++
curl_global_init(CURL_GLOBAL_ALL);
curl = curl_easy_init();
std::string url = "https://ejemplo.com/api";
```

Definimos también una variable para almacenar los datos de la solicitud en formato JSON, si es necesario.

```C++
std::string data = "{ \"nombre\": \"Juan\", \"apellido\": \"García\" }";
```

Ahora, configuramos nuestra solicitud con la función `curl_easy_setopt()`, especificando la URL, el tipo de solicitud y los datos que queremos enviar.

```C++
curl_easy_setopt(curl, CURLOPT_URL, url.c_str()); // URL del servidor
curl_easy_setopt(curl, CURLOPT_CUSTOMREQUEST, "POST"); // Tipo de solicitud
curl_easy_setopt(curl, CURLOPT_POSTFIELDS, data.c_str()); // Datos de la solicitud
```

Finalmente, ejecutamos la solicitud con `curl_easy_perform()` y obtenemos la respuesta del servidor con `curl_easy_getinfo()`.

```C++
res = curl_easy_perform(curl);
long response_code;
curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &response_code);
```

Si la solicitud se ha enviado correctamente, `res` tendrá un valor igual a `CURLE_OK` y `response_code` contenerá el código de respuesta del servidor.

## Profundizando

Existen muchas otras opciones y configuraciones que se pueden aplicar a una solicitud HTTP utilizando `libcurl`, como por ejemplo:

- Especificar headers personalizados
- Enviar y recibir archivos
- Autenticación
- Funciones de callback para gestionar eventos
- Solicitud en paralelo
- Entre otros.

Además, también se pueden utilizar otras librerías para enviar solicitudes HTTP en C++, como `libmicrohttpd` o `Boost.Asio`. Todas estas opciones ofrecen flexibilidad y control en el envío y recepción de datos a través de HTTP.

## Ver también

- [Documentación oficial de libcurl en español](https://curl.haxx.se/libcurl/c/libcurl-tutorial.html)
- [Ejemplos prácticos de solicitudes HTTP en C++](https://www.progville.com/cplusplus/http-client-cpp-libcurl/)
- [Librería Boost.Asio para enviar solicitudes HTTP en C++](https://www.boost.org/doc/libs/1_75_0/doc/html/boost_asio/example/cpp03/http/client/sync_client.cpp)