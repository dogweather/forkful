---
title:                "C#: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Enviar uma requisição HTTP é fundamental quando se trabalha com APIs e comunicação entre sistemas. Essa é uma forma de enviar uma mensagem a um servidor e receber uma resposta, permitindo que dados sejam trocados de forma eficiente e organizada.

## Como Fazer

Para enviar uma requisição HTTP em C#, podemos utilizar a classe `HttpClient` da biblioteca `System.Net.Http`. Primeiro, é preciso instanciar um objeto dessa classe e definir a URL da requisição. Em seguida, podemos adicionar cabeçalhos e parâmetros, caso necessário, e então enviar a requisição utilizando o método `GetAsync` ou `PostAsync`, dependendo do tipo de requisição.

Exemplo de código para enviar uma requisição GET usando o `HttpClient`:

```C#
var cliente = new HttpClient();
string url = "https://exemplo.com/api/usuarios";
var requisicao = await cliente.GetAsync(url);

// verifica se a requisição foi bem sucedida
if (requisicao.IsSuccessStatusCode) 
{
  // obtém o conteúdo da resposta
  var conteudo = await requisicao.Content.ReadAsStringAsync();
  Console.WriteLine(conteudo); // imprime o conteúdo no console
}
```

Ao enviar uma requisição, é possível receber diferentes tipos de resposta, como texto, JSON ou até mesmo um arquivo. Para manipular esses dados, é importante entender o formato da resposta e utilizar as ferramentas adequadas para processá-la.

## Aprofundando no Assunto

Para entender melhor o processo de envio de requisições HTTP em C#, é importante conhecer os principais atributos e métodos da classe `HttpClient`. Alguns deles são:

- `BaseUrl`: define a base URL da requisição, podendo ser usada em todas as chamadas;
- `Headers`: permite adicionar cabeçalhos à requisição, como `User-Agent` e `Content-Type`;
- `DefaultRequestHeaders`: semelhante ao `Headers`, mas com opção para definir cabeçalhos para todas as requisições criadas através do `HttpClient`;
- `GetStringAsync`: método auxiliar para realizar uma requisição GET e receber uma resposta em formato de texto.

Para receber a resposta de uma requisição, podemos utilizar o método `SendAsync` e armazenar o resultado em um objeto do tipo `HttpResponseMessage`. A partir desse objeto, é possível acessar informações como código de status, cabeçalhos e conteúdo da resposta.

## Veja Também

- [Documentação oficial do `HttpClient` em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient?view=net-5.0)
- [Tutorial sobre envio de requisições HTTP em C#](https://www.treinaweb.com.br/blog/como-enviar-requisicoes-http-em-c/)