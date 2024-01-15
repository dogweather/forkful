---
title:                "Enviando uma solicitação http com autenticação básica."
html_title:           "Kotlin: Enviando uma solicitação http com autenticação básica."
simple_title:         "Enviando uma solicitação http com autenticação básica."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica é uma forma segura de autenticar um usuário em um aplicativo ou site. Isso garante que apenas usuários autorizados tenham acesso a determinados recursos ou informações.

## Como fazer:

```Kotlin
val url = URL("https://www.example.com/login")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"

val userCredentials = "username:password"
val basicAuth = "Basic " + Base64.encodeToString(userCredentials.toByteArray(), Base64.DEFAULT)

connection.setRequestProperty("Authorization", basicAuth)

val responseCode = connection.responseCode
```

Saída do código acima:

```
200 OK
```

## Deep Dive:

Quando se trata de autenticação básica em uma solicitação HTTP, o header "Authorization" é fundamental. Ele contém o tipo de autenticação, que neste caso é "Basic", e as credenciais do usuário, que são o nome de usuário e a senha concatenados e codificados em Base64.
Na classe HttpURLConnection do Kotlin, o método `setRequestProperty()` é usado para definir esse header. No entanto, é importante lembrar que as credenciais são enviadas em texto simples, por isso é recomendável usar o protocolo HTTPS para criptografar a comunicação.

## Veja também:

- [Documentação oficial do Kotlin sobre HttpURLConnection](https://kotlinlang.org/api/latest/jvm/stdlib/java.net.-u-r-l-connection/request-method.html)
- [Tutorial do Baeldung sobre autenticação básica em solicitações HTTP](https://www.baeldung.com/java-http-request-basic-authentication)