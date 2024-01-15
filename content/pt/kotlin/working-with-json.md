---
title:                "Trabalhando com json"
html_title:           "Kotlin: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-json.md"
---

{{< edit_this_page >}}

## Por que usar JSON em suas aplicações Kotlin?

JSON (JavaScript Object Notation) é um formato de dados popular para troca de informações entre sistemas. Ele é leve, fácil de ler e escrever, e é compatível com muitas linguagens de programação. Ao utilizar JSON em suas aplicações Kotlin, você pode facilmente armazenar e recuperar dados estruturados de uma forma organizada e eficiente.

## Como trabalhar com JSON em Kotlin

Para começar a trabalhar com JSON em suas aplicações Kotlin, você precisará importar a biblioteca "kotlinx.serialization". Ela fornece funções e classes úteis para codificar e decodificar dados JSON. Veja um exemplo abaixo de como codificar uma classe de usuário para JSON:

```Kotlin
import kotlinx.serialization.*
import kotlinx.serialization.json.*

@Serializable
data class User(val name: String, val age: Int)

val user = User("João", 25)
val json = Json.encodeToString(user)

println(json)
```
Saída:
`{"name":"João","age":25}`

Para decodificar um JSON de volta para um objeto Kotlin, podemos usar a função "decodeFromString". Veja o exemplo abaixo:

```Kotlin
val userFromJson = Json.decodeFromString<User>(json)

println(userFromJson.name)
```

Saída:
`João`

Você também pode trabalhar com estruturas de dados mais complexas, como listas e mapas. Veja um exemplo de como codificar e decodificar uma lista de usuários para JSON:

```Kotlin
val userList = listOf(User("João", 25), User("Maria", 30))

val jsonList = Json.encodeToString(userList)

println(jsonList)
```
Saída:
`[{"name":"João","age":25},{"name":"Maria","age":30}]`

## Aprofundando no uso de JSON em Kotlin

Ao trabalhar com JSON em Kotlin, é importante ter em mente alguns conceitos-chave, como codificação e decodificação, chaves e valores, e tipos de dados suportados. Você pode se aprofundar nesses conceitos e aprender mais sobre as opções e recursos disponíveis na biblioteca "kotlinx.serialization" acessando a documentação oficial em seu site.

Se você está trabalhando com uma API que retorna dados em formato JSON, é importante também ter uma boa compreensão de como fazer requisições HTTP em Kotlin. Uma opção popular para isso é utilizar a biblioteca "OkHttp", que oferece uma interface simples e eficiente para trabalhar com requisições e respostas HTTP.

## Veja também

Para mais informações sobre como trabalhar com JSON em Kotlin, você pode conferir estes links:

- [Documentação da biblioteca kotlinx.serialization em português](https://kotlinlang.org/docs/serialization.html#serialization-json)
- [Exemplo de uso de JSON em uma API utilizando a biblioteca OkHttp](https://proandroiddev.com/working-with-httpclient-okhttp-in-kotlin-3a8f10055d01)
- [Tutorial de como trabalhar com JSON em Kotlin no Android Studio](https://www.raywenderlich.com/7038677-kotlin-serialization-tutorial-getting-started#toc-anchor-001)