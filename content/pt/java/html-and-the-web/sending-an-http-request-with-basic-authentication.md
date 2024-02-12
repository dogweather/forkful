---
title:                "Enviando uma requisição HTTP com autenticação básica"
aliases: - /pt/java/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:11.997467-07:00
model:                 gpt-4-1106-preview
simple_title:         "Enviando uma requisição HTTP com autenticação básica"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Enviar uma requisição HTTP com autenticação básica é o processo de acessar um recurso web protegido, incluindo um cabeçalho de autorização com um nome de usuário e senha codificados. Programadores utilizam isso para se conectar de forma segura a APIs ou serviços que requerem verificação de identidade.

## Como fazer:

```java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.Base64;

public class BasicAuthExample {
    public static void main(String[] args) {
        // Configura o endereço do recurso e as credenciais
        String url = "http://example.com/api/resource";
        String username = "user";
        String password = "pass";

        // Codifica as credenciais em base64
        String encodedCredentials = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());

        // Cria uma requisição com o cabeçalho de autenticação
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Basic " + encodedCredentials)
                .build();
        
        // Envia a requisição utilizando o cliente HTTP
        HttpClient client = HttpClient.newHttpClient();
        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```

Exemplo de Saída:
```
{ "name": "Valor Exemplo", "description": "Uma resposta JSON de um recurso com autenticação básica." }
```

## Mergulho Profundo

Historicamente, a autenticação básica é um método antiquado porém ainda em uso devido à sua simplicidade. Ela envia o nome de usuário e senha codificados em Base64, mas não criptografados, o que significa que, se interceptados, podem ser decodificados facilmente. Por isso, é essencial usar HTTPS em vez de HTTP ao enviar requisições de autenticação básica.

Como alternativas, temos as autenticações Digest e OAuth, que oferecem mais segurança. A autenticação Digest envia uma hash criptografada das credenciais do usuário, enquanto OAuth usa tokens de acesso que podem ser revogados a qualquer momento sem a necessidade de alterar as senhas dos usuários.

Na implementação, ao enviar uma requisição HTTP com autenticação básica em Java, o uso de `HttpClient` simplifica o processo ao gerenciar a conexão. Usar `Authenticator` e `PasswordAuthentication` são opções também, para quem prefere mais controle sobre o processo de autenticação.

## Veja Também

- [Documentação oficial do HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Tutorial de autenticação OAuth](https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2)
