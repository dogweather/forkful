---
title:                "C: Descargando una página web"
simple_title:         "Descargando una página web"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por qué

La descarga de páginas web puede ser una tarea muy útil para los programadores de C. Con este proceso, se pueden obtener datos actualizados en tiempo real o guardar información útil para su uso posterior. Además, es una excelente forma de aprender sobre programación web y cómo funciona Internet.

## Cómo hacerlo
Para descargar una página web en C, se requiere utilizar la biblioteca de sockets. Primero, se debe crear un socket TCP utilizando la función `socket()`. A continuación, se deben especificar la dirección del servidor y el puerto mediante la función `bind()`. Luego, se establece una conexión con el servidor utilizando la función `connect()`. Una vez establecida la conexión, se pueden enviar solicitudes HTTP al servidor utilizando la función `send()`. Finalmente, se recibirá la respuesta del servidor utilizando la función `recv()` y se puede guardar la página web en un archivo o visualizarla en la terminal.

A continuación se muestra un ejemplo de código para descargar y guardar una página web en un archivo llamado "webpage.html":

```C
#include <stdio.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>

int main() {
    int sockfd, port, bytes, file;
    char hostname[] = "www.example.com";
    char request[] = "GET / HTTP/1.1\r\nHost: www.example.com\r\n\r\n";
    char buffer[4096];
    struct sockaddr_in server_addr;
    struct hostent *server;

    // Crear un socket TCP
    sockfd = socket(AF_INET, SOCK_STREAM, 0);

    // Obtener dirección IP del servidor
    server = gethostbyname(hostname);

    // Especificar dirección y puerto del servidor
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(80);
    bcopy((char *)server->h_addr, (char *)&server_addr.sin_addr.s_addr, server->h_length);

    // Establecer conexión con el servidor
    connect(sockfd, (struct sockaddr *)&server_addr, sizeof(server_addr));

    // Enviar solicitud HTTP al servidor
    send(sockfd, request, strlen(request), 0);

    // Recibir respuesta del servidor y guardarla en un archivo
    file = open("webpage.html", O_CREAT | O_WRONLY, 0666);
    while ((bytes = recv(sockfd, buffer, 4096, 0)) > 0) {
        write(file, buffer, bytes);
    }

    // Cerrar conexión y archivo
    close(sockfd);
    close(file);

    return 0;
}
```

## Profundizando
Además de la descarga de páginas web, también se pueden realizar otras acciones utilizando sockets en C, como el envío y recepción de correos electrónicos, el uso de aplicaciones de mensajería instantánea o incluso la conexión a juegos en línea. La biblioteca de sockets proporciona una gran cantidad de funciones y opciones para trabajar con conexiones de red, por lo que es muy útil para una amplia gama de aplicaciones.

## Ver también
- [Documentación de la biblioteca de sockets en C](https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/sys_socket.h.html)
- [Tutorial de programación de sockets en C](https://www.geeksforgeeks.org/socket-programming-cc/) 
- [Ejemplos de descarga de páginas web en C](https://github.com/kiritigowda/Download-a-Webpage-in-C-Language)