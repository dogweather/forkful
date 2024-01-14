---
title:                "Java: Ndosando uma solicitação http com autenticação básica"
simple_title:         "Ndosando uma solicitação http com autenticação básica"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Por que enviar uma solicitação HTTP com autenticação básica em Java?

Enviar solicitações HTTP com autenticação básica é uma prática comum em desenvolvimento web, especialmente quando se trata de se comunicar com APIs. Ao incluir credenciais de autenticação em uma solicitação, é possível garantir a segurança dos dados que estão sendo transmitidos.

## Como fazer

Para enviar uma solicitação HTTP com autenticação básica em Java, é necessário primeiro importar a classe HttpURLConnection. Em seguida, é preciso criar uma instância dessa classe e definir o URL e o método de solicitação (GET, POST, PUT, DELETE). Em seguida, podemos usar o método setRequestProperty() para adicionar as credenciais de autenticação à solicitação. Por fim, é só usar o método getResponseCode() para obter o código de resposta da solicitação.

```Java
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

public class HttpBasicAuthentication {

    public static void main(String[] args) {

        try {
            // Criar instância de HttpURLConnection
            URL url = new URL("https://exemplo.com/api");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();

            // Definir método da solicitação
            con.setRequestMethod("GET");
            
            // Adicionar credenciais de autenticação
            String username = "usuario";
            String password = "senha";
            String credentials = username + ":" + password;
            String auth = "Basic " + java.util.Base64.getEncoder().encodeToString(credentials.getBytes());
            con.setRequestProperty("Authorization", auth);

            // Obter código de resposta da solicitação
            int responseCode = con.getResponseCode();
            System.out.println("Código de resposta: " + responseCode);

        } catch (IOException e) {
            System.out.println("Erro: " + e.getMessage());
        }
    }
}
```

Exemplo de saída:

```
Código de resposta: 200
```

## Aprofundamento

Ao enviar uma solicitação HTTP com autenticação básica, é importante lembrar de usar o protocolo de segurança HTTPS para garantir que as credenciais não sejam enviadas em texto claro. Além disso, é recomendável armazenar as credenciais em um local seguro e não enviar continuamente as mesmas credenciais em cada solicitação.

## Veja também

- [Documentação oficial da classe HttpURLConnection em Java](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Tutorial de autenticação básica em Java](https://www.baeldung.com/java-http-request-basic-authentication)