---
title:                "C: Enviando una solicitud http con autenticación básica"
simple_title:         "Enviando una solicitud http con autenticación básica"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por qué

Enviar una solicitud HTTP con autenticación básica es una forma común de autenticar y autorizar las solicitudes a un servidor. Esto es especialmente importante en aplicaciones web y servicios en línea que manejan información confidencial y requieren medidas de seguridad robustas. Al utilizar la autenticación básica, se solicita al usuario un nombre de usuario y contraseña antes de acceder a la información protegida.

## Cómo hacerlo

Para enviar una solicitud HTTP con autenticación básica en C, se necesita un programa que pueda comunicarse con un servidor a través de la red utilizando sockets. A continuación, se pueden seguir los siguientes pasos generales:

1. Crear un socket TCP/IP y conectarse al servidor especificando su dirección IP y puerto.
2. Codificar la solicitud HTTP específica que se desea enviar (por ejemplo, GET, PUT, POST) utilizando la sintaxis adecuada.
3. Incluir las credenciales de autenticación básica en la solicitud codificando el nombre de usuario y contraseña en formato Base64.
4. Enviar la solicitud al servidor y esperar una respuesta.
5. Decodificar la respuesta y procesarla según sea necesario.

A continuación se muestra un ejemplo de código en C que utiliza la librería OpenSSL para enviar una solicitud HTTP con autenticación básica:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <openssl/ssl.h>

int main() {
   // Crear socket TCP/IP
   int sockfd = socket(AF_INET, SOCK_STREAM, 0);
   struct sockaddr_in server;
   server.sin_family = AF_INET;
   server.sin_addr.s_addr = inet_addr("dirección_IP_del_servidor");
   server.sin_port = htons(80);

   // Conexión al servidor
   connect(sockfd, (struct sockaddr *)&server, sizeof(server));

   // Crear contexto SSL
   SSL_CTX *ctx;
   ctx = SSL_CTX_new(SSLv23_client_method());
   SSL_CTX_set_options(ctx, SSL_OP_NO_SSLv2);

   // Conexión segura SSL al servidor
   SSL *ssl;
   ssl = SSL_new(ctx);
   SSL_set_fd(ssl, sockfd);
   SSL_connect(ssl);

   // Codificar credenciales de autenticación básica en Base64
   char credentials[] = "usuario:contraseña";
   unsigned char base64_credentials[80];
   BIO *b64 = BIO_new(BIO_f_base64());
   BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
   BIO *bio = BIO_new(BIO_s_mem());
   BIO_push(b64, bio);
   BIO_write(b64, credentials, strlen(credentials));
   BIO_flush(b64);
   char *temp = (char*)malloc(80*sizeof(char));
   memset(temp, '\0', 80);
   BIO_read(b64, temp, strlen(credentials));
   BIO_free_all(b64);
   temp[strlen(temp)] = '\n';
   strcat((char*)base64_credentials, (const char*)temp);

   // Crear solicitud codificada con Basic Auth
   char request[200];
   strcpy(request, "GET /ruta/recurso HTTP/1.1\r\n");
   strcat(request, "Host: servidor.com\r\n");
   strcat(request, "Authorization: Basic ");
   strcat(request, (const char*)base64_credentials);
   strcat(request, "Connection: close\r\n\r\n");

   // Enviar solicitud al servidor
   int bytes_sent = SSL_write(ssl, request, strlen(request));

   // Recibir respuesta del servidor
   char response[200];
   int bytes_received = SSL_read(ssl, response, sizeof(response));

   // Imprimir respuesta
   printf("Respuesta del servidor:\n%s\n", response);

   // Cerrar conexión y liberar recursos
   SSL_shutdown(ssl);
   SSL_free(ssl);
   SSL_CTX_free(ctx);
   close(sockfd);

   return 0;
}
```

La salida de este programa sería algo similar a:

```
Respuesta del servidor:
HTTP/1.1 200 OK
Server: Apache
Content-Type: text/html
Content-Length: 1346
Connection: close
...
...
```

## Profundizando

La autenticación básica funciona mediante la codificación de las credenciales en formato Base64 y su inclusión en la cabecera de la solicitud HTTP. Esta forma de autenticación es relativamente simple y ampliamente compatible con la mayoría de los servidores web, pero no es muy segura ya que las credenciales se envían en texto plano. En su lugar, se recomienda utilizar un sistema más seguro como HTTPS para prote