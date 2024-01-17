---
title:                "Enviando una solicitud http"
html_title:           "C: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Enviar una solicitud HTTP es una forma en que los programadores pueden comunicarse con servidores web y obtener información de ellos. Los programadores usan este método para realizar peticiones específicas al servidor y recibir una respuesta en formato de texto.

## Cómo:

```C
#include <stdio.h>
#include <curl/curl.h> //librería para enviar solicitudes HTTP

void main() {
   
   CURL *curl; //declaración de variable CURL
   CURLcode res; //variable para almacenar el código de respuesta
  
   curl = curl_easy_init(); //inicializar la variable curl
   if(curl) { 
      
      //establecer la URL de la página web a la que queremos enviar la solicitud
      curl_easy_setopt(curl, CURLOPT_URL, "https://ejemplo.com/"); 
  
      //enviar la solicitud y guardar el código de respuesta
      res = curl_easy_perform(curl); 

      //imprimir el código de respuesta
      printf("Código de respuesta: %d\n", res); 
      
      //cerrar la solicitud y liberar la memoria
      curl_easy_cleanup(curl);
   }
  
}


```

El código anterior nos permite enviar una solicitud HTTP utilizando la librería CURL en C. Primero, se declara una variable CURL y un código de respuesta. Luego, se inicializa la variable y se establece la URL a la que queremos enviar la solicitud. Finalmente, se envía la solicitud y se imprime el código de respuesta.

## Inmersión profunda:

Enviar solicitudes HTTP ha sido una herramienta fundamental para los programadores desde la creación de la World Wide Web en 1989. Aunque puede haber otras formas de comunicarse con servidores web, como a través de sockets, enviar solicitudes HTTP es la forma más común y sencilla de hacerlo. La librería CURL que se utiliza en el ejemplo también es compatible con varios protocolos de transferencia de datos, lo que la convierte en una herramienta versátil para enviar y recibir información.

## Ver también:

- [Documentación oficial de la librería CURL](https://curl.se/libcurl/)
- [Introducción a solicitudes HTTP](https://developer.mozilla.org/es/docs/Web/HTTP/Overview)