---
title:                "C: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar um pedido HTTP em um programa em C?

Ao criar um programa em C, pode ser necessário interagir com um servidor, seja para obter informações ou enviar dados. Nesses casos, é comum utilizar um protocolo de comunicação chamado HTTP (Hypertext Transfer Protocol). Enviar um pedido HTTP é uma forma de se comunicar com um servidor e obter ou enviar informações.

## Como enviar um pedido HTTP em um programa em C

Para enviar um pedido HTTP em um programa em C, é necessário seguir algumas etapas:

1. Inicialize uma conexão com o servidor utilizando a função `socket()`.
2. Crie a estrutura de dados `sockaddr_in` para armazenar as informações do servidor e o número da porta a ser utilizado.
3. Utilize a função `connect()` para estabelecer a conexão com o servidor.
4. Formate o cabeçalho da requisição HTTP, especificando o verbo, o caminho do recurso e a versão do protocolo.
5. Envie o cabeçalho e quaisquer dados adicionais usando a função `send()`.
6. Receba a resposta do servidor utilizando a função `recv()`.
7. Analise a resposta e extraia as informações necessárias.

Veja um exemplo de código abaixo:

```C
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>

#define PORT 80
#define HOST "www.exemplo.com"

int main(void) {
    // Inicializar a conexão
    int client_socket = socket(AF_INET, SOCK_STREAM, 0);
    
    // Criar a estrutura com as informações do servidor
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(PORT);
    server_addr.sin_addr.s_addr = inet_addr(HOST);
    
    // Conectar ao servidor
    connect(client_socket, (struct sockaddr *) &server_addr, sizeof(server_addr));
    
    // Formatar o cabeçalho da requisição
    char *request = "GET / HTTP/1.1\r\nHost: www.exemplo.com\r\n\r\n";
    
    // Enviar a requisição
    send(client_socket, request, strlen(request), 0);
    
    // Receber a resposta
    char buffer[1024] = {0};
    recv(client_socket, buffer, 1024, 0);
    
    // Imprimir a resposta
    printf("%s\n", buffer);
    
    // Fechar a conexão
    close(client_socket);
    
    return 0;
}
```

A saída do código acima seria algo como:

```
HTTP/1.1 200 OK
Date: Fri, 01 Oct 2021 10:20:00 GMT
Server: Apache/2.4.29 (Ubuntu)
Content-Length: 29
Connection: close
Content-Type: text/html; charset=UTF-8

<html><body>Exemplo de página</body></html>
```

## Detalhes sobre o envio de um pedido HTTP em um programa em C

Um pedido HTTP consiste em um cabeçalho, seguido de qualquer dado adicional que possa ser necessário. O cabeçalho é composto por:

- Um verbo, que indica a ação a ser realizada (GET, POST, PUT, DELETE, etc).
- O caminho do recurso, que especifica qual recurso está sendo solicitado.
- A versão do protocolo, que define qual versão do HTTP está sendo utilizada.

Além disso, o cabeçalho também pode conter informações adicionais, como cabeçalhos de autenticação, cookies, entre outros.

Ao enviar um pedido HTTP, é importante garantir que o cabeçalho esteja formatado corretamente, seguindo as especificações do protocolo. Além disso, é necessário tratar possíveis erros de conexão e recebimento de dados do servidor.

## Veja também

Para saber mais sobre como enviar um pedido HTTP em um programa em C, confira os links abaixo:

- [Tutorial: Envio de requisições HTTP em C](https://www.programiz.com/article/http-request-c)
- [Documentação da função `socket()`](https://pubs.opengroup.org/onlinepubs/009695399/functions/socket.html)
- [Documentação da função `connect()`](https://pubs.opengroup.org/onlinepubs/009695399/functions/connect.html)
- [Documentação da função `send()`](https://pubs.opengroup