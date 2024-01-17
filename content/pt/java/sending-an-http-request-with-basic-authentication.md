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

# O que é & Por quê?

Enviar uma requisição HTTP com autenticação básica é uma maneira de garantir a segurança em aplicações web. Ao adicionar credenciais de usuário (como um nome de usuário e senha) à requisição, os programadores podem garantir que apenas usuários autorizados terão acesso aos dados solicitados.

# Como fazer:

### Exemplo 1:
```java
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthenticationExample {

    public static void main(String[] args) {

        // Definir URL da requisição
        String url = "https://www.example.com/api/data";

        // Definir credenciais de usuário
        String username = "usuario";
        String password = "senha";

        // Codificar as credenciais em Base64
        String encodedCredentials = Base64.getEncoder()
                .encodeToString((username + ":" + password).getBytes(StandardCharsets.UTF_8));

        try {
            // Criar conexão HTTP
            URL obj = new URL(url);
            HttpURLConnection con = (HttpURLConnection) obj.openConnection();

            // Definir método da requisição
            con.setRequestMethod("GET");

            // Adicionar a chave "Authorization" com as credenciais codificadas no header da requisição
            con.setRequestProperty("Authorization", "Basic " + encodedCredentials);

            // Obter resposta da requisição
            int responseCode = con.getResponseCode();
            System.out.println("Código de resposta: " + responseCode);

        } catch (Exception e) {
            e.printStackTrace();
        }

    }
}
```

### Saída esperada:
```
Código de resposta: 200
```

# Mais Detalhes:

### Contexto Histórico:
A autenticação básica foi definida em 1999 no RFC 2617 e é uma forma simples de autenticar usuários em aplicações web. Embora seja menos segura do que outras formas de autenticação (como OAuth), ainda é amplamente utilizada em sistemas legados.

### Alternativas:
Uma alternativa mais segura para a autenticação básica é o uso de tokens de acesso, como no protocolo OAuth. Nesse caso, as credenciais do usuário não são enviadas a cada requisição, o que reduz o risco de exposição em caso de ataque ou interceptação da comunicação.

### Detalhes de Implementação:
No exemplo acima, as credenciais do usuário são codificadas em Base64 e adicionadas ao header da requisição usando a chave "Authorization". O servidor recebe as credenciais e realiza a autenticação, retornando uma resposta de sucesso ou erro.

# Veja também:

- RFC 2617: https://tools.ietf.org/html/rfc2617
- OAuth: https://oauth.net/2/