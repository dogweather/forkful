---
title:                "Elixir: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Quando se está construindo uma aplicação da web, é inevitável que você precise fazer requests HTTP para buscar dados de outras fontes ou para interagir com APIs. Por isso, é importante entender como enviar um request HTTP usando a linguagem Elixir.

## Como Fazer

1. Primeiro, precisamos utilizar o módulo HTTPoison para fazer os requests HTTP. Para isso, precisamos adicionar o HTTPoison ao nosso arquivo de dependências do projeto. No arquivo mix.exs, adicione a seguinte linha dentro do bloco de funções (deps):

```Elixir
{:httpoison, "~> 1.0"}
```

2. Então, podemos chamar o HTTPoison com a função HTTPoison.request/5, que recebe cinco argumentos: o método HTTP, a URL, o cabeçalho (header), o corpo (body) e a opção (options). Por exemplo:

```Elixir
response = HTTPoison.request(:get, "https://jsonplaceholder.typicode.com/posts/1", headers, nil)
```

Nesse exemplo, estamos fazendo um request GET para a API do JSONPlaceholder para buscar o primeiro post.

3. Para ver a resposta do request, podemos usar o recurso Pattern Matching no Elixir para extrair os dados que precisamos do response, que é uma tupla com o status do request e o corpo da resposta. Veja esse exemplo:

```Elixir
{:ok, %{body: body}} = response
IO.puts(body)
```

Isso irá imprimir o corpo da resposta na tela.

## Deep Dive

Além do HTTPoison, existem outras bibliotecas e opções para fazer requests HTTP em Elixir, como Tesla, Gun e muitas outras. Algumas dessas bibliotecas podem ter recursos como pooling de conexões, suporte a websockets e compressão de dados.

Outra coisa importante a se considerar é a manipulação de erros. Ao fazer um request HTTP, muitas coisas podem dar errado, como conexão perdida, tempo limite de resposta atingido ou dados corrompidos. É importante estar ciente desses possíveis erros e lidar com eles de forma adequada no seu código.

## Veja Também

- [Documentação do HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Exemplos de códigos com requests HTTP em Elixir](https://github.com/evadne/elixir-conduit)
- [Outras opções para fazer requests HTTP em Elixir](https://elixir.libhunt.com/categories/743-http-client)