---
title:                "Kotlin: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que enviar uma solicitação HTTP com autenticação básica?

Enviar uma solicitação HTTP com autenticação básica é um método seguro e confiável de se comunicar com um servidor. Isso garante que apenas usuários autorizados tenham acesso aos dados e recursos protegidos pelo servidor. 

## Como fazer:

```Kotlin
val url = URL("https://exemplo.com/api/recurso")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"

// Codificando nome de usuário e senha em Base64
val name = "usuario"
val password = "senha"
val authString = "$name:$password"
val authStringEnc = Base64.getEncoder().encodeToString(authString.toByteArray())

connection.setRequestProperty("Authorization", "Basic $authStringEnc")

// Lendo a resposta
val bufferedReader = BufferedReader(InputStreamReader(connection.inputStream))
val response = StringBuffer()
var inputLine = bufferedReader.readLine()
while(inputLine !=null){
    response.append(inputLine)
    inputLine = bufferedReader.readLine()
}

bufferedReader.close()

// Imprimindo o resultado
println(response.toString())

```

**Saída:**

```
{"status":200,"message":"Recurso enviado com sucesso!"}
```

## Mergulho Profundo:

Ao enviar uma solicitação HTTP com autenticação básica, é importante usar o cabeçalho "Authorization" para incluir as credenciais do usuário codificadas em Base64. Este cabeçalho também deve conter a palavra "Basic" seguida por um espaço, para indicar a codificação utilizada. Além disso, é necessário fornecer a URL correta do servidor e o método de solicitação correto, como GET, POST ou PUT.

Outro fator importante a considerar é a segurança das credenciais de autenticação básica. Como elas são codificadas, elas podem ser facilmente decodificadas por alguém com conhecimento técnico. Portanto, é aconselhável usar a autenticação básica apenas em conexões seguras, como HTTPS.

## Veja também:

- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/reference/)
- [Tutorial de autenticação básica HTTP em Java](https://www.baeldung.com/java-http-request-basic-authentication)
- [Artigo sobre codificação Base64 em Kotlin](https://www.vogella.com/tutorials/Kotlin/article.html)