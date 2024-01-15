---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Java: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Muitas vezes, ao desenvolver um aplicativo ou site, é necessário que o usuário se autentique para acessar determinadas informações ou funcionalidades. O envio de uma solicitação HTTP com autenticação básica é uma forma eficaz de garantir a autenticidade do usuário e proteger os dados sensíveis.

## Como Fazer

Para enviar uma solicitação HTTP com autenticação básica em Java, é necessário seguir alguns passos simples:

1. Importe a classe `java.net.HttpURLConnection`, que fornece os métodos necessários para realizar a solicitação HTTP.
2. Crie uma instância da classe `HttpURLConnection` usando o método `openConnection()` e passe a URL da API ou do serviço que deseja acessar como parâmetro.
3. Defina o tipo de solicitação, método e cabeçalho de autenticação usando os métodos `setRequestMethod()` e `setRequestProperty()`.
4. Codifique o nome de usuário e a senha em Base64 usando a classe `java.util.Base64`.
5. Adicione o cabeçalho de autenticação à solicitação usando o método `setRequestProperty()` novamente, desta vez passando o cabeçalho `Authorization` e a string codificada em Base64.
6. Execute a solicitação usando o método `connect()` e obtenha a resposta usando o método `getResponseCode()`.
7. Se a resposta for bem-sucedida (código 200), os dados solicitados estarão disponíveis e podem ser lidos usando os métodos `getInputStream()` e `getOutputStream()`.

Um exemplo de código completo pode ser visto abaixo:

```Java
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class HTTPRequestExample {

   public static void main(String[] args) {

      try {
         // Cria uma URL com o endereço da API ou serviço desejado
         URL url = new URL("https://api.meuservico.com/dados");

         // Abre uma conexão HTTP usando a URL
         HttpURLConnection con = (HttpURLConnection) url.openConnection();

         // Define o método de requisição e cabeçalho de autenticação
         con.setRequestMethod("GET");
         con.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("usuario:senha".getBytes()));

         // Executa a conexão e obtém a resposta
         con.connect();
         int responseCode = con.getResponseCode();

         // Verifica se a resposta foi bem-sucedida
         if (responseCode == 200) {
            // Lê os dados da resposta e faz o tratamento necessário
            // ...

            // Fecha a conexão
            con.disconnect();
         }
      } catch (IOException e) {
         // Trata possíveis erros de conexão
         e.printStackTrace();
      }
   }
}
```

## Deep Dive

A autenticação básica utiliza o cabeçalho `Authorization` para enviar as credenciais codificadas em Base64 para o servidor. Quando a solicitação é recebida, o servidor decodifica as informações e verifica se o usuário e a senha são válidos.

Embora a autenticação básica seja uma forma simples de proteger serviços HTTP, ela não é muito segura, pois as credenciais são enviadas em texto simples e podem ser facilmente interceptadas por um atacante. Por isso, é recomendável utilizar outros métodos de autenticação mais robustos quando possível.

## Veja Também
- Documentação oficial do Java para classe `java.net.HttpURLConnection`: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html
- Tutorial de autenticação HTTP básica com Java: https://www.baeldung.com/java-http-request#basic-authentication
- Vídeo explicando como funciona a autenticação básica em HTTP: https://www.youtube.com/watch?v=OZmBGn90ny8