---
aliases:
- /pt/kotlin/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:21.681785-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica significa\
  \ incluir credenciais de usu\xE1rio e senha codificadas em base64 no cabe\xE7alho\
  \ da requisi\xE7\xE3o.\u2026"
lastmod: 2024-02-18 23:08:58.100767
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica significa\
  \ incluir credenciais de usu\xE1rio e senha codificadas em base64 no cabe\xE7alho\
  \ da requisi\xE7\xE3o.\u2026"
title: "Enviando uma requisi\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Enviar uma requisição HTTP com autenticação básica significa incluir credenciais de usuário e senha codificadas em base64 no cabeçalho da requisição. Programadores fazem isso para acessar recursos protegidos em um servidor.

## Como Fazer:
```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val urlConnection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray())

    urlConnection.requestMethod = "GET"
    urlConnection.setRequestProperty("Authorization", "Basic $encodedCredentials")

    val responseCode = urlConnection.responseCode
    println("Response Code: $responseCode")
    if (responseCode == HttpURLConnection.HTTP_OK) {
        urlConnection.inputStream.bufferedReader().use {
            it.lines().forEach { line -> println(line) }
        }
    } else {
        println("Failed to authenticate.")
    }
}

fun main() {
    val testUrl = "http://your-protected-resource.com"
    val username = "yourUsername"
    val password = "yourPassword"
    
    sendGetRequestWithBasicAuth(testUrl, username, password)
}
```
Saída esperada:
```
Response Code: 200
{Aqui estarão os dados obtidos da requisição, caso seja bem-sucedida}
```

## Mergulho Profundo
A autenticação básica é um método antigo, mas ainda amplamente utilizado devido à sua simplicidade. Esse método envia as credenciais no cabeçalho da requisição, por isso é extremamente importante usar HTTPS para proteger as informações contra interceptações. Em Kotlin, essa funcionalidade geralmente é implementada via `HttpURLConnection` ou com bibliotecas de terceiros como OkHttp, que pode fornecer uma abstração mais segura e mais rica de funcionalidades.

Existem alternativas mais seguras, como a autenticação Digest, OAuth e tokens JWT que oferecem camadas adicionais de segurança e são preferidas em aplicações modernas.

Detalhes de implementação a considerar:

1. Codificar sempre as credenciais em base64.
2. Tratar corretamente os códigos de resposta HTTP para garantir que o programa responda apropriadamente a falhas de autenticação.
3. Considerar o gerenciamento cuidadoso das credenciais para evitar vazamentos de informação.

## Veja Também
- [OkHttp](https://square.github.io/okhttp/) - Uma biblioteca cliente HTTP para Kotlin e Java.
- [Ktor Client](https://ktor.io/docs/client.html) - Cliente assíncrono para Kotlin.
- [Base64 Encoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html) - Referência de codificação Base64 da Oracle.
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617) - Documentação oficial do esquema de autenticação básica.
