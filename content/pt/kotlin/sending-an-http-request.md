---
title:                "Enviando uma solicitação http"
html_title:           "Kotlin: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Enviar uma requisição HTTP é uma forma de comunicação entre uma aplicação e um servidor web. Os programadores fazem isso para obter informações, enviar dados, ou interagir com sistemas e serviços externos.

## Como Fazer:

```Kotlin 
import java.net.URL

fun main() {
    val url = URL("https://www.example.com") // cria um objeto URL com o endereço do servidor
    val conn = url.openConnection() // abre uma conexão com a URL
    val response = conn.getInputStream().bufferedReader().use { it.readText() } // lê a resposta do servidor
    println(response) // imprime a resposta no console
}
```

A saída deste código será o conteúdo da página https://www.example.com .

## Profundando Mais:

Enviar uma requisição HTTP é um método amplamente utilizado na programação para a comunicação entre aplicações web e serviços externos. Alternativas para as conexões HTTP incluem FTP (File Transfer Protocol) e SMTP (Simple Mail Transfer Protocol), mas esses protocolos são menos usados devido à popularidade e eficiência do HTTP.

Na implementação de uma requisição HTTP, o programa primeiro estabelece uma conexão com o servidor usando um objeto URL, especificando o endereço. Em seguida, a conexão é aberta e a resposta do servidor é lida e retornada para o programa. Existem bibliotecas e frameworks que podem facilitar a realização de requisições HTTP em aplicações Kotlin, como o Ktor e o Retrofit.

## Veja Também:

- [Documentação oficial do Kotlin sobre requisições HTTP](https://kotlinlang.org/docs/tutorials/networking.html)
- [Ktor: framework de cliente HTTP para Kotlin](https://ktor.io/)
- [Retrofit: biblioteca para fazer requisições HTTP de forma declarativa em Kotlin](https://square.github.io/retrofit/)