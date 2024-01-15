---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "C#: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Por que

Há muitas razões pelas quais você pode querer enviar uma solicitação HTTP com autenticação básica. Talvez você precise acessar uma API que requer autenticação para obter certos dados. Ou talvez você precise fazer com que seu aplicativo se comunique com um servidor usando um nome de usuário e senha para acesso seguro. De qualquer forma, aprender como enviar uma solicitação HTTP com autenticação básica é essencial para muitos aplicativos e projetos.

## Como Fazer

Para começar a enviar uma solicitação HTTP com autenticação básica em C#, você precisará ter acesso à biblioteca System.Net.Http e usar a classe HttpClient para enviar sua solicitação. Aqui está um exemplo de código que você pode usar como ponto de partida:

```C#
using System;
using System.Net.Http;
using System.Text;

class Program
{
    static async Task Main()
    {
        var username = "seunomeusuario";
        var password = "suasenha";
        var url = "https://www.exemplo.com/api";

        var httpClient = new HttpClient();

        var clientCredentials = new UTF8Encoding().GetBytes($"{username}:{password}");
        var base64ClientCredentials = Convert.ToBase64String(clientCredentials);
        
        var request = new HttpRequestMessage(HttpMethod.Get, url);
        request.Headers.Add("Authorization", $"Basic {base64ClientCredentials}");

        var response = await httpClient.SendAsync(request);

        Console.WriteLine($"Resposta: {await response.Content.ReadAsStringAsync()}");
    }
}
```

Neste exemplo, você primeiro precisa obter o nome de usuário e senha para a autenticação básica. Em seguida, você precisa criar uma string codificada em base64 dessas informações, que será usada como valor do cabeçalho de autorização em sua solicitação. Ao usar o objeto HttpClient, você pode enviar a solicitação e receber a resposta. Você precisará verificar a documentação da API que está chamando para entender os detalhes específicos da autenticação básica necessária.

## Mergulho Profundo

Ao enviar uma solicitação HTTP com autenticação básica, é importante entender como esse processo funciona nos bastidores. A autenticação básica é um dos métodos de autenticação mais antigos e simples. Ele envia o nome de usuário e senha codificados em base64 no cabeçalho de autorização da solicitação, usando o formato "Basic [string codificada]". É importante estar ciente de que, embora a base64 seja uma forma de codificar dados para fins de transporte, ela não é considerada uma forma segura de criptografar informações confidenciais, pois pode facilmente ser decodificada. Portanto, antes de usar a autenticação básica em seu aplicativo, certifique-se de pesquisar as melhores práticas para manter suas informações de autenticação seguras.

## Veja Também

- [Documentação oficial do HttpClient em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Guia de autenticação básica no HTTP](https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Authentication#basic_authentication_scheme)
- [Artigo sobre autenticação em APIs REST em C#](https://www.researchgate.net/publication/331104733_Autenticacao_em_APIs_REST_utilizando_o_framework_RESTSharp)