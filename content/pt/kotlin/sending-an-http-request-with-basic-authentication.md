---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar um pedido HTTP com autenticação básica é a prática de transmitir um pedido para um servidor web enquanto se identifica através de um nome de usuário e uma senha. Programadores fazem isso para interagir com APIs protegidas que exigem credenciais válidas para aceitar pedidos.

## Como fazer:
Aqui está um exemplo rápido de como enviar um pedido HTTP GET com autenticação básica usando a biblioteca Ktor em Kotlin.

```Kotlin
val client = HttpClient() {
    install(Auth) {
        basic {
            username = "nomeDeUsuario"
            password = "senha"
        }
    }
}

val httpResponse: HttpResponse = client.get("https://meusite.com")
println(httpResponse.status)  // Imprime o status do pedido HTTP no console
client.close()  
```

## Aprofundando
1. **Contexto Histórico:** A autenticação básica HTTP foi um dos primeiros métodos para controlar o acesso a recursos da web. Apesar de não ser a opção mais segura, pelo seu método simples de requisitar usuário e senha, é ainda bastante usada nos dias de hoje.

2. **Alternativas:** Para segurança melhorada, muitos optam por autenticação digest ou por token, como o JWT (JSON Web Token).

3. **Detalhes de Implementação:** Na autenticação básica, as credenciais são codificadas em Base64 e incluídas em cada pedido HTTP. Entretanto, isso não é seguro, pois se alguém interceptar a comunicação, pode decodificar facilmente as informações. É recomendado usar HTTPS para garantir que as informações do usuário sejam encriptadas durante a transmissão.

## Veja Também:
- Documentação Oficial do Ktor: https://ktor.io/clients/http-client/features/authentication.html
- Autenticação Digest: https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#autenticação_digest
- Autenticação JWT: https://jwt.io/introduction/