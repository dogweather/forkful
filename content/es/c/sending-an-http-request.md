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

## Por qué
Enviar solicitudes HTTP es una parte fundamental de la programación en C, ya que te permite comunicarte con servidores y obtener información de diversas fuentes. Además, es necesario para desarrollar aplicaciones web y servicios en línea.

## Cómo hacerlo
Enviar una solicitud HTTP en C es un proceso sencillo que requiere seguir algunos pasos básicos.

### Codificación
Primero, debes incluir la biblioteca estándar `stdio.h` y la biblioteca de entrada y salida de red `netdb.h` en tu programa.

```C
#include <stdio.h>
#include <netdb.h>
```

Luego, debes establecer una variable de tipo `int` para almacenar el descriptor de archivo de la conexión al servidor y una struct llamada `addrinfo` para almacenar información sobre la conexión.

```C
int sockfd;
struct addrinfo hints, *servinfo;
```

A continuación, debes inicializar la struct `hints` con cero y establecer valores para el miembro `ai_family` que indica la familia de protocolos que se utilizará y el miembro `ai_socktype` que especifica el tipo de socket que se utilizará para la conexión.

```C
memset(&hints, 0, sizeof(hints));
hints.ai_family = AF_UNSPEC;
hints.ai_socktype = SOCK_STREAM;
```

Después, debes llamar a la función `getaddrinfo()` para obtener información sobre la conexión al servidor. Esta función acepta cuatro argumentos: el nombre del host, el número de puerto, un puntero a la struct `hints` y un puntero a la struct `servinfo`.

```C
getaddrinfo("www.example.com", "80", &hints, &servinfo);
```

A continuación, debes crear un socket utilizando la información obtenida por `getaddrinfo()` y almacenar su descriptor de archivo en la variable `sockfd`.

```C
sockfd = socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol);
```

Finalmente, debes utilizar la función `connect()` para establecer una conexión al servidor utilizando el descriptor de archivo del socket y la información obtenida por `getaddrinfo()`.

```C
connect(sockfd, servinfo->ai_addr, servinfo->ai_addrlen);
```

### Ejemplo completo
Aquí está un ejemplo completo de un programa en C que envía una solicitud HTTP GET a un servidor y muestra la respuesta en la consola.

```C
#include <stdio.h>
#include <netdb.h>

int main() {
  // Variables para almacenar el descriptor de archivo y la información de conexión
  int sockfd;
  struct addrinfo hints, *servinfo;

  // Inicializar la struct hints con cero
  memset(&hints, 0, sizeof(hints));

  // Establecer valores para ai_family y ai_socktype
  hints.ai_family = AF_UNSPEC;
  hints.ai_socktype = SOCK_STREAM;

  // Obtener información de conexión utilizando getaddrinfo()
  getaddrinfo("www.example.com", "80", &hints, &servinfo);

  // Crear un socket y almacenar su descriptor de archivo
  sockfd = socket(servinfo->ai_family, servinfo->ai_socktype, servinfo->ai_protocol);

  // Establecer una conexión utilizando connect()
  connect(sockfd, servinfo->ai_addr, servinfo->ai_addrlen);

  // Enviar una solicitud GET al servidor
  char request[] = "GET / HTTP/1.1\r\n\r\n";
  send(sockfd, request, strlen(request), 0);

  // Almacenar la respuesta en un búfer
  char response[1024];
  recv(sockfd, response, sizeof(response), 0);

  // Imprimir la respuesta en la consola
  printf("%s", response);

  // Cerrar la conexión y liberar la memoria
  close(sockfd);
  freeaddrinfo(servinfo);

  return 0;
}
```

### Resultado
Si ejecutas el programa anterior, deberías obtener una respuesta similar a esta:

```
HTTP/1.1 200 OK
Date: Sun, 16 May 2021 16:45:24 GMT
Server: Apache/2.4.23 (Unix)
Last-Modified: Tue, 29 Nov 2016 21:35:54 GMT
ETag: "2f-5427e1e1c9f56"
Accept-Ranges: bytes
Content-Length: 47
Content-Type: text/html

<html><body><h1>It works!</h1></body></html>
```

## Deep Dive
Para enviar una solicitud HTTP en C, neces