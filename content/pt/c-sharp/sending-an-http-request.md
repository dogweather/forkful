---
title:                "Enviando uma solicitação http"
html_title:           "C#: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar um pedido HTTP (Hypertext Transfer Protocol) é uma maneira de um programador se comunicar com um servidor web. Isso significa que eles estão pedindo por algum tipo de informação ou serviço do servidor. Os programadores geralmente fazem isso para criar aplicativos e sites interativos para os usuários.

## Como fazer:

```C#
var url = "https://www.exemplo.com/pedido"; // URL do servidor
var client = new HttpClient(); // Inicializa o cliente HTTP

// Envia um pedido GET para a URL do servidor e aguarda a resposta
var response = await client.GetAsync(url);
// Lê a resposta como uma string
var result = await response.Content.ReadAsStringAsync();

// Imprime a resposta no console
Console.WriteLine(result);
```

Output:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Exemplo</title>
</head>
<body>
  <h1>Obrigado por enviar um pedido!</h1>
  <p>Seu pedido foi processado com sucesso.</p>
</body>
</html>
```

## Detalhes importantes:

Antes de enviar um pedido HTTP, é importante conhecer os diferentes tipos de métodos de pedido, como GET, POST, PUT e DELETE, e quando cada um deve ser usado. Além disso, é necessário entender os códigos de status da resposta do servidor, como 200 para "OK" e 404 para "Não encontrado". Existem também várias bibliotecas e APIs que podem ser usadas em vez de codificar manualmente um pedido HTTP.

## Veja também:

- [Documentação oficial do HttpClient (em inglês)](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/http-requests?view=aspnetcore-3.1)
- [Uma introdução ao HTTP (em português)](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Overview)