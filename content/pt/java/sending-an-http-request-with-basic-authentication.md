---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Enviar uma solicitação HTTP com autenticação básica é o processo de se comunicar com um servidor de forma segura, passando um nome de usuário e senha codificados em base64 no cabeçalho. Os programadores fazem isso para validar a identidade e garantir a segurança ao acessar recursos protegidos em um servidor.

## Como Fazer:

Vamos ver um exemplo de envio de uma solicitação HTTP GET com autenticação básica usando a biblioteca HttpClient do Java. Aqui está o código:

```Java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        String encodeToString = Base64.getEncoder().encodeToString(("username:password").getBytes(StandardCharsets.UTF_8));
        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://example.com"))
                .header("Authorization", "Basic " + encodeToString)
                .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.body());
    }
}
```
Este exemplo mostra a saída da resposta do corpo recebido após enviar a solicitação HTTP GET.

## Aprofundar

A autenticação básica HTTP é um esquema de autenticação antigo, mas ainda amplamente usado devido à sua simplicidade. No entanto, tem um problema principal - as credenciais são passadas como um texto simples codificado em base64 e podem ser facilmente decodificadas se interceptadas.

Existem alternativas para a autenticação básica, como a autenticação por tokens JWT e o OAuth, que são mais seguras e geralmente preferidas para aplicações maiores ou mais sensíveis.

A implementação é bastante direta, como visto acima. Também pode-se usar bibliotecas externas como Unirest ou OkHttp, que simplificam o processo e proporcionam mais flexibilidade.

## Veja Também:

Para uma compreensão mais aprofundada, consulte os seguintes recursos:

1. [Java HTTP Client Documentation] (https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
2. [Baeldung Guide to Basic Authentication with HttpClient] (https://www.baeldung.com/httpclient-basic-authentication-java)
3. [Mozilla Developer Network (MDN) web docs on HTTP Authentication] (https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
4. [JWT Authentication Tutorial] (https://www.toptal.com/java/rest-security-with-jwt-spring-security-and-java)