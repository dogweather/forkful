---
title:                "Enviando una solicitud http con autenticación básica"
html_title:           "Arduino: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué? 

Enviar una solicitud HTTP con autenticación básica es enviar una solicitud a un servidor web con un nombre de usuario y una contraseña codificados en Base64 en los encabezados de la solicitud. Los programadores lo hacen para acceder de manera segura y remota a los recursos protegidos de un servidor.

## Cómo hacerlo:

Para ello, podemos usar la biblioteca libcurl en C.

Primero, instalas libcurl:

```c
 sudo apt-get install libcurl4-openssl-dev
```

Aquí tenemos un ejemplo de solicitud HTTP con autenticación básica en C:

```c
 #include <curl/curl.h>

 int main(void)
 {
   CURL *curl;
   CURLcode res;

   curl_global_init(CURL_GLOBAL_DEFAULT);

   curl = curl_easy_init();
   if(curl) {
     curl_easy_setopt(curl, CURLOPT_URL, "https://example.com");

     curl_easy_setopt(curl, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
     curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
     curl_easy_setopt(curl, CURLOPT_PASSWORD, "pass");

     res = curl_easy_perform(curl);

     if(res != CURLE_OK)
       fprintf(stderr, "error: %s\n",
               curl_easy_strerror(res));
     
     curl_easy_cleanup(curl);
   }

   curl_global_cleanup();

   return 0;
 }
```
La salida de muestra podría ser el html de la página web solicitada, o un mensaje de error si las credenciales proporcionadas son incorrectas.

## Buceo Profundo

La autenticación básica con HTTP es un mecanismo que ha estado en uso desde las primeras etapas de la web. Pero este método de autenticación es simple y no es completamente seguro. A menos que se use HTTPS, las credenciales se envían en texto plano, lo que podría provocar un riesgo de seguridad.

Existen alternativas más seguras disponibles hoy en día, como el uso de tokens JWT o autenticación con OAuth2.0. Sin embargo, la autenticación básica sigue siendo útil para algunos casos de uso.

Cuando se implementa la autenticación básica en C con libcurl, libcurl se encarga de codificar las credenciales en Base64 y agregarse a los encabezados HTTP, lo que facilita la tarea.

## Ver También

- [Documentación oficial de libcurl](https://curl.se/libcurl/)
- [W3C - HTTP Authentication](https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html)
- [Autenticación JWT](https://jwt.io/introduction/)
- [OAuth 2.0](https://oauth.net/2/)