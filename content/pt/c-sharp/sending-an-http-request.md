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

## Por que enviar uma solicitação HTTP?

Enviar uma solicitação HTTP é necessário para o funcionamento de muitos aplicativos e websites modernos. É uma forma de se comunicar com servidores e obter as informações necessárias para exibir conteúdo dinâmico, fazer login em contas, entre outros.

## Como fazer?

Existem várias maneiras de enviar uma solicitação HTTP em C#, mas vamos nos concentrar no método mais comum usando o objeto HttpClient. Primeiro, precisamos instalar o pacote `System.Net.Http` no nosso projeto. Em seguida, vamos utilizar o seguinte código:

```C#
// Importando o namespace necessário
using System.Net.Http;

// Criando uma instância do HttpClient
var client = new HttpClient();

// Enviando a solicitação e obtendo a resposta
HttpResponseMessage response = await client.GetAsync("https://exemplo.com");

// Lendo o conteúdo da resposta
string conteudo = await response.Content.ReadAsStringAsync();

// Exibindo o conteúdo na tela
Console.WriteLine(conteudo);
```

O código acima envia uma solicitação GET para o endereço especificado e exibe o conteúdo da resposta. Mas é importante lembrar que, dependendo da API ou website que estamos utilizando, pode ser necessário enviar dados de autenticação ou parâmetros adicionais. Nesses casos, devemos utilizar a classe `HttpRequestMessage` para criar nossa solicitação personalizada.

## Aprofundando-se

Além do método `GetAsync` utilizado no exemplo anterior, o objeto `HttpClient` também possui métodos para enviar solicitações HTTP do tipo POST, PUT, DELETE, entre outros. Além disso, podemos especificar cabeçalhos de requisição, definir um timeout para a conexão e tratar eventuais erros na resposta.

Também é importante mencionar que, em uma aplicação real, devemos sempre utilizar as melhores práticas de segurança ao enviar e receber solicitações HTTP. Isso inclui utilizar autenticação segura e criptografar os dados da requisição para evitar possíveis ataques.

## Veja também

- [Documentação oficial da classe HttpClient em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient)
- [Tutorial sobre requisições HTTP em C#](https://www.tutlane.com/tutorial/csharp/csharp-httprequest-and-httpresponse)
- [Como utilizar autenticação em requisições HTTP em C#](https://www.talkingdotnet.com/how-to-get-authentication-done-in-an-asp-net-core-api-after-upgrading-to-net-core-3-0/)