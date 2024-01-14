---
title:                "Kotlin: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP?

Enviar solicitações HTTP é uma parte essencial do desenvolvimento de aplicativos modernos. Isso permite que nossos aplicativos se comuniquem com servidores e troquem dados de forma rápida e eficiente. Sem enviar solicitações HTTP, nossos aplicativos não seriam capazes de se conectar com o mundo exterior.

## Como fazer
Para enviar uma solicitação HTTP em Kotlin, usamos a biblioteca nativa HttpURLConnection. Primeiro, criamos uma URL com o endereço do servidor que desejamos enviar a solicitação. Em seguida, configuramos a conexão HttpURLConnection e definimos o método de solicitação (GET, POST, PUT, DELETE). Finalmente, usamos a conexão para obter uma InputStream com a resposta do servidor.
```
Kotlin 
val url = URL("http://www.exemplo.com/api/usuarios")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
val stream = connection.inputStream
```
Para interpretar a resposta do servidor, podemos usar um InputStreamReader e um BufferedReader, que nos permitem ler o conteúdo da resposta. Podemos então fazer o parsing do conteúdo usando as classes Json do Kotlin ou qualquer outra biblioteca de sua escolha.

## Abrindo a caixa de ferramentas: Enviando solicitações HTTP com Retrofit
Se você está procurando uma abordagem mais elegante e simples para lidar com solicitações HTTP em seu aplicativo Kotlin, o Retrofit é a biblioteca certa para você. Retrofit é uma biblioteca de cliente HTTP, que oferece uma camada de abstração sobre a API HttpURLConnection, tornando mais fácil para nós trabalharmos com solicitações e respostas HTTP.

Para usar o Retrofit, primeiro precisamos adicionar a dependência ao nosso arquivo Gradle. Em seguida, criamos uma interface com os métodos necessários para nossas solicitações. Especificamos a URL base e o caminho para a chamada em cada método. Finalmente, usamos o Retrofit para criar uma instância de nossa Interface e fazemos as solicitações de forma mais simplificada.

```
Kotlin
interface ApiUsuario {
    @GET("/usuarios")
    fun getUsuarios(): Call<List<Usuario>>
}

// Instancia o Retrofit
val retrofit = Retrofit.Builder()
    .baseUrl("http://www.exemplo.com/api/")
    .addConverterFactory(GsonConverterFactory.create())
    .build()

// Cria a instância da interface
val apiUsuario = retrofit.create(ApiUsuario::class.java)

// Faz a chamada
val call = apiUsuario.getUsuarios()

```
Uma vez que nos familiarizamos com o Retrofit, podemos usá-lo para enviar solicitações mais complexas, como adicionar cabeçalhos, parâmetros de consulta e até mesmo lidar com erros de forma mais eficiente. 

## Veja também
- [Documentação oficial do Retrofit](https://square.github.io/retrofit/)
- [Como fazer solicitações HTTP em Kotlin](https://www.developer.com/lang/scala/making-http-requests-in-kotlin/)
- [Biblioteca HttpURLConnection](https://developer.android.com/reference/java/net/HttpURLConnection)