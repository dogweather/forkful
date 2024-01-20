---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando uma Solicitação HTTP em Java

## O Que & Por Quê?
Enviar uma solicitação HTTP significa pedir dados a um servidor remoto. Os programadores fazem isso quando precisam se comunicar com um servidor remoto, seja para enviar ou solicitar dados.

## Como fazer:
Vamos fazer isso com a nova API de HttpClient do Java 11.

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
 
public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("https://exemplo.com"))
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.body());
    }
}
```
A resposta aqui será o conteúdo primário de "https://exemplo.com".

## Mergulho Profundo
Java adicionou `HttpClient` no Java 11 para finalmente substituir `HttpURLConnection` por um método mais moderno e flexível para lidar com solicitações HTTP.

Existem outras alternativas além do `HttpClient` padrão do Java. Por exemplo, há bibliotecas como OkHttp e Apache HttpClient que podem oferecer mais funcionalidades.

`HttpClient` é bastante flexível. Ele permite GET, POST, e outros métodos HTTP. Você pode adicionar cabeçalhos personalizados, enviar dados no corpo, lidar com redirecionamentos e muito mais.

## Veja também
- [Documentação oficial do HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [OkHttp](https://square.github.io/okhttp/)