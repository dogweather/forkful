---
title:                "Enviando uma solicitação HTTP com autenticação básica."
html_title:           "Kotlin: Enviando uma solicitação HTTP com autenticação básica."
simple_title:         "Enviando uma solicitação HTTP com autenticação básica."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar uma solicitação HTTP com autenticação básica é um procedimento comum em programação. Isso é usado para permitir que um usuário se conecte a um servidor protegido por senha. Os programadores realizam essa ação para garantir a segurança dos dados ao acessar uma aplicação.

## Como fazer:

```Kotlin
val url = URL("https://example.com") // cria uma URL para o servidor
val connection = url.openConnection() as HttpURLConnection // abre uma conexão HTTP
connection.setRequestProperty("Authorization", "Basic " + "username:password".encodeBase64()) // define a autenticação básica
val responseCode = connection.responseCode // obtém o código de resposta
if (responseCode == HttpURLConnection.HTTP_OK) { // se o código de resposta for OK, a conexão foi bem sucedida
    val input = connection.inputStream // obtém a entrada de dados do servidor
    // processa os dados recebidos
} else {
    // lidar com erro de conexão
}
```

## Profundidade:

A autenticação básica foi introduzida em 1996 como um meio de realizar a autenticação HTTP. No entanto, devido à vulnerabilidade de segurança, é recomendado usar a autenticação básica apenas em redes de servidor seguro ou em conjunto com outros sistemas de autenticação.

Existem muitos outros mecanismos de autenticação, como OAuth e Token Authentication, que são mais seguros e eficazes do que a autenticação básica. É importante que os programadores escolham o método de autenticação adequado para suas aplicações, levando em consideração a segurança e a praticidade.

Para implementar a autenticação básica em Kotlin, é necessário entender como funciona o processo de codificação Base64 e como enviar cabeçalhos personalizados nas solicitações HTTP. Além disso, é importante garantir que o servidor esteja configurado corretamente para aceitar autenticação básica.

## Veja também:

- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/reference/)
- [Tutorial sobre codificação Base64 em Kotlin](https://www.baeldung.com/java-base64-encode-and-decode)
- [Explicação detalhada sobre autenticação básica em redes HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#Basic_authentication_scheme)