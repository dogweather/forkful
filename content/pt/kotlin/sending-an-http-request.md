---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Enviando uma solicitação HTTP em Kotlin

## O quê e por quê?

Enviar uma solicitação HTTP é uma maneira de obter dados de um servidor web. Programadores fazem isso para acessar informações fora de seus próprios sistemas, seja uma API de clima, um serviço de música ou a API do Twitter.

## Como fazer:

Vamos usar a biblioteca Ktor para enviar uma solicitação HTTP. Primeiro adicione a dependência ao seu arquivo `build.gradle`:

```Kotlin
dependencies {
    implementation "io.ktor:ktor-client-core:1.6.3"
    implementation "io.ktor:ktor-client-cio:1.6.3"
}
```
Aqui está um exemplo de código para enviar uma solicitação GET:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()
    val webpage: String = client.get("https://ktor.io/")
    println(webpage)
    client.close()
}
```
Quando você executa isso, verá o HTML da página retornada no console.

## Mergulho profundo

A especificação HTTP nasceu em 1991 e evoluiu muito desde então, mas a ideia básica de enviar solicitações para um servidor persiste. Alternativas para HTTP incluem gRPC (do Google) e GraphQL (do Facebook), mas para muitos casos de uso, HTTP (especificamente HTTP/1.1 e HTTP/2) ainda é uma ótima solução.

Quando falamos de enviar um HTTP request em Kotlin, podemos usar várias bibliotecas, além do Ktor, como OkHttp ou Fuel. Cada um tem suas próprias vantagens, mas escolhemos o Ktor por ser totalmente baseado em Kotlin e ter ótima integração com as corrotinas do Kotlin.

A solicitação HTTP em si envolve o envio de um pacote de dados para um servidor (o "request") e a obtenção de um pacote de volta (a "response"). Esse processo é sincrônico, mas pode ser executado de maneira assíncrona em Kotlin usando corrotinas.

## Veja também:

- Documentação do Ktor: https://ktor.io/clients/http-client/
- Guia de OkHttp: https://square.github.io/okhttp/
- Documentação do Fuel: https://fuel.gitbook.io/documentation/