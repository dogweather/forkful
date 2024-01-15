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

## Por que enviar uma requisição HTTP?

Enviar uma solicitação HTTP é uma das tarefas mais básicas e comuns ao criar aplicativos e sites modernos. Ao enviar uma requisição, podemos obter informações de um servidor ou enviar dados para serem processados. É uma parte essencial na comunicação entre o cliente e o servidor na internet.

## Como enviar uma requisição HTTP com Kotlin

Para enviar uma requisição HTTP em Kotlin, podemos utilizar a biblioteca nativa do Kotlin, chamada "kotlinx.serialization". Esta biblioteca permite o uso de objetos Kotlin diretamente como dados JSON a serem enviados em uma requisição.

Vamos criar um exemplo de como enviar uma requisição GET usando a API do GitHub para obter informações de um usuário. Primeiro, importe a biblioteca "kotlinx.serialization" em seu projeto. Em seguida, crie uma classe com as informações do usuário que você deseja obter, por exemplo:

```
data class Usuario(
    val nome: String,
    val empresa: String,
    val localizacao: String
)
```

Em seguida, importe as dependências necessárias para a requisição, como a biblioteca "okhttp" e o "json.kt" que contém a classe criada anteriormente. Em seguida, podemos criar um OkHttpClient e um Gson para nos ajudar no processo de serialização de objetos Kotlin para JSON:

```
val client = OkHttpClient()
val gson = Gson()
val request = Request.Builder()
    .url("https://api.github.com/users/johnsmith")
    .build()
```

Agora, podemos enviar a requisição e obter a resposta do servidor usando o método "newCall" do OkHttpClient:

```
client.newCall(request).execute().use { response ->
    if (!response.isSuccessful) throw IOException("Unexpected code $response")
    val responseData = response.body!!.string()
    val usuario = gson.fromJson(responseData, Usuario::class.java)
}
```

Podemos então acessar as informações do usuário desejado a partir do objeto "usuario" criado, como por exemplo:

```
println(usuario.nome)
println(usuario.empresa)
println(usuario.localizacao)
```

O código completo seria semelhante a este:

```
    val client = OkHttpClient()
    val gson = Gson()
    val request = Request.Builder()
        .url("https://api.github.com/users/johnsmith")
        .build()
    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Unexpected code $response")
        val responseData = response.body!!.string()
        val usuario = gson.fromJson(responseData, Usuario::class.java)
        println(usuario.nome)
        println(usuario.empresa)
        println(usuario.localizacao)
    }
```

## Mergulho aprofundado em requisições HTTP

Enquanto este artigo aborda somente uma maneira de enviar uma requisição HTTP em Kotlin, existem outras bibliotecas e abordagens disponíveis para essa tarefa. Além disso, é importante ter em mente os diferentes tipos de métodos HTTP, seus propósitos e suas diferenças, como GET, POST, PUT, DELETE, entre outros. Além disso, é importante também entender os diferentes cabeçalhos (headers) que podem ser utilizados em uma requisição HTTP, como o cabeçalho de autenticação ou o cabeçalho de tipo de conteúdo (Content-Type).

## Veja também
- [Biblioteca kotlinx.serialization para serialização de objetos em JSON](https://github.com/Kotlin/kotlinx.serialization)
- [Biblioteca OkHttp para realizar requisições HTTP em Kotlin](https://github.com/square/okhttp)
- [Tutorial de Kotlin para iniciantes](https://kotlinlang.org/docs/tutorials/)