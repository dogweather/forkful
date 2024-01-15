---
title:                "Enviando una solicitud Http con autenticación básica"
html_title:           "C: Enviando una solicitud Http con autenticación básica"
simple_title:         "Enviando una solicitud Http con autenticación básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué 
 En la programación moderna, enviar solicitudes HTTP es una tarea común. Una forma de asegurar la comunicación entre el cliente y el servidor es a través de la autenticación básica, que permite el acceso limitado a ciertos recursos en una aplicación web. En este artículo, aprenderemos cómo enviar una solicitud HTTP con autenticación básica utilizando C.

## Cómo hacerlo 
La biblioteca estándar de C proporciona una función `curl` para enviar solicitudes HTTP. Sin embargo, para habilitar la autenticación básica, necesitamos agregar algunos encabezados personalizados a nuestra solicitud. Aquí hay un ejemplo de cómo enviar una solicitud HTTP GET con autenticación básica utilizando C: 

```C
#include <stdio.h>
#include <curl/curl.h>

int main(void)
{
  CURL *curl;
  CURLcode response_code;

  // Inicializar la biblioteca curl 
  curl = curl_easy_init();
  if(curl) {
    // Establecer la URL del servidor al que deseamos enviar la solicitud 
    curl_easy_setopt(curl, CURLOPT_URL, "https://example.com/resources");
    
    // Habilitar la autenticación básica y proporcionar las credenciales de usuario y contraseña
    curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
    curl_easy_setopt(curl, CURLOPT_USERPWD, "usuario:contraseña");

    // Enviar la solicitud GET 
    response_code = curl_easy_perform(curl);

    // Revisar si la solicitud fue procesada correctamente 
    if(response_code != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() fallido: %s\n",
              curl_easy_strerror(response_code));
    
    // Limpiar la memoria utilizada 
    curl_easy_cleanup(curl);
  }

  return 0;
}
```

En este ejemplo, estamos utilizando la biblioteca `curl` para realizar una solicitud GET al servidor `example.com/resources`. El servidor requiere autenticación básica, por lo que agregamos los encabezados personalizados `HTTPAUTH` y `USERPWD` a nuestra solicitud. Esto asegura que la solicitud sea procesada correctamente y se obtenga una respuesta del servidor.

## Profundizando
La autenticación básica es un método simple de autenticación HTTP, que utiliza una combinación de usuario y contraseña para verificar la identidad del cliente. La idea es que el cliente envíe las credenciales codificadas en base64 en los encabezados de la solicitud. Una desventaja de la autenticación básica es que la información de inicio de sesión se envía en texto plano, lo que la hace vulnerable a ataques de intercepción.

Para mejorar la seguridad, se recomienda utilizar otros métodos de autenticación más avanzados como OAuth o JWT. Sin embargo, si solo necesitas asegurar una conexión simple entre el cliente y el servidor, la autenticación básica puede ser una buena solución.

## Ver también
- [Documentación oficial de cURL](https://curl.se/libcurl/c/http-auth.html)
- [Cómo enviar solicitudes HTTP desde C en Stack Overflow](https://stackoverflow.com/questions/2329573/c-libcurl-get-output-into-a-string)
- [Artículo sobre autenticación básica en aplicaciones web](https://www.imperva.com/learn/application-security/basic-authentication/)