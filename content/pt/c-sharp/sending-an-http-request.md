---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O Que & Por quê?:

Enviar uma solicitação HTTP é um método usado pelos programas para solicitar dados de um servidor. Os programadores fazem isso quando precisam acessar, criar, atualizar ou excluir informações em um servidor remoto.

## Como fazer: 

A maneira mais simples e direta de enviar uma requisição HTTP em C# é usando a classe HttpClient. Aqui está um exemplo de código:

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    private static readonly HttpClient client = new HttpClient();

    static async Task Main()
    {
        var response = await client.GetAsync("http://example.com");

        Console.WriteLine(response.StatusCode);
    }
}
```
Neste exemplo, seu programa fará uma solicitação HTTP GET para "http://example.com" e imprimirá o status da resposta. 

## Mergulho Profundo:

Historicamente, .NET tem suporte para solicitações HTTP desde a primeira versão. Originalmente, havia `WebRequest` e `WebResponse`, mas agora a maneira recomendada é usar `HttpClient`.

Existem várias alternativas para enviar solicitações HTTP. Além do HttpClient, existem bibliotecas de terceiros que oferecem uma interface mais rica ou simples, como a RestSharp ou Flurl.

Em relação à implementação, uma coisa importante a lembrar é que `HttpClient` é projetado para ser reutilizado. Deve-se evitar criar novas instâncias de HttpClient para cada solicitação para evitar o esgotamento de sockets. Em vez disso, reutilize a mesma instância para todas as solicitações.

## Veja Também: 

1. [Documentação oficial do HttpClient](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient?view=net-5.0)
2. [Biblioteca RestSharp](https://restsharp.dev/)
3. [Biblioteca Flurl](https://flurl.dev/)
4. [Artigo sobre reutilização do HttpClient](https://docs.microsoft.com/pt-br/dotnet/architecture/microservices/implement-resilient-applications/use-httpclientfactory-to-implement-resilient-http-requests)