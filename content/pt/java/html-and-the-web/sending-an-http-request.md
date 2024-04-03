---
date: 2024-01-20 17:59:58.337241-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP \xE9 o processo de solicitar dados\
  \ ou a\xE7\xE3o de outro servidor atrav\xE9s da internet. Programadores fazem isso\
  \ para interagir com\u2026"
lastmod: '2024-03-13T22:44:46.452961-06:00'
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP \xE9 o processo de solicitar dados ou a\xE7\
  \xE3o de outro servidor atrav\xE9s da internet."
title: "Enviando uma requisi\xE7\xE3o HTTP"
weight: 44
---

## How to:
O Java oferece várias maneiras de enviar requisições HTTP. Desde o Java 11, a classe `HttpClient` tornou o processo mais simples e direto. Aqui está um exemplo rápido de como fazer uma requisição GET:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpRequestExample {
    public static void main(String[] args) {
        // Cria o HttpClient
        HttpClient client = HttpClient.newHttpClient();

        // Cria a requisição
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://example.com"))
                .build();

        // Envia a requisição e recebe a resposta
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```

Esse código vai imprimir o conteúdo HTML da página `http://example.com`.

## Deep Dive:
As requisições HTTP não são novidade; elas são a espinha dorsal da web desde os anos 90. Antes do `HttpClient` no Java 11, a tarefa muitas vezes exigia bibliotecas de terceiros como Apache HttpComponents ou a classe `HttpURLConnection`. Cada alternativa tem suas peculiaridades e casos de uso, mas `HttpClient` é mais moderno e integrado, dispensando dependências externas.

Falando de níveis mais baixos: uma requisição HTTP é apenas um texto formatado enviado por um socket TCP. O `HttpClient` e as classes relacionadas simplificam esse processo, lidando com a serialização, análise e erros de conexão para você.

O uso de `sendAsync` no exemplo permite a execução assíncrona, não bloqueando o programa enquanto espera pela resposta do servidor. Para requisições síncronas, use `send`.

Para aprofundar, é fundamental entender os métodos HTTP (GET, POST, PUT, DELETE, etc.) e status de resposta (200 OK, 404 Not Found, 500 Internal Server Error, etc.), além de como trabalhar com cabeçalhos e corpos de requisição.

## See Also:
- A documentação oficial da classe `HttpClient`: https://docs.oracle.com/en/java/javase/17/docs/api/java.net.http/java/net/http/HttpClient.html
- Tutorial da Oracle: https://docs.oracle.com/javase/tutorial/networking/urls/index.html
- RFC 7230, a especificação detalhada do protocolo HTTP/1.1: https://tools.ietf.org/html/rfc7230
