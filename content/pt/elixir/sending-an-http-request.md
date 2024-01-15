---
title:                "Enviando um pedido http."
html_title:           "Elixir: Enviando um pedido http."
simple_title:         "Enviando um pedido http."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Por que

Você já se perguntou como os sites e aplicativos conseguem se comunicar? Bem, a resposta é: através do protocolo HTTP. E se você é um programador Elixir, aprender como enviar uma solicitação HTTP pode abrir portas para explorar novas funcionalidades e integrações em seus projetos.

## Como fazer

Enviar uma solicitação HTTP em Elixir é bastante simples e aqui está um exemplo básico:

```Elixir
{:ok, response} = HTTPoison.get("https://api.example.com")
IO.inspect(response.status_code)
```

Neste exemplo, usamos a biblioteca HTTPoison, que é uma das formas mais populares de fazer solicitações HTTP em Elixir. Começamos importando a biblioteca com `use HTTPoison.Base` e em seguida, chamamos o método `get` com a URL como argumento. A resposta que recebemos é uma tupla, com o status da resposta na primeira posição e o conteúdo na segunda. Em seguida, usamos a função `IO.inspect` para imprimir o código de status da resposta.

Mas isso é apenas o básico. Você também pode adicionar cabeçalhos e parâmetros de consulta à sua solicitação usando os opcionais `headers` e `query` na função `HTTPoison.get`.

```Elixir
{:ok, response} = HTTPoison.get("https://api.example.com", headers: [{"Authorization", "Token abc123"}], query: [limit: 10])
```

Além disso, você também pode fazer solicitações POST e PUT usando o método `post` e `put`, respectivamente. E não se preocupe com tratamento de erros, pois a biblioteca já lida com isso e retorna uma mensagem de erro caso algo dê errado.

## Mergulho profundo

Enquanto a biblioteca HTTPoison é uma ótima opção para fazer solicitações HTTP em Elixir, também existem outras bibliotecas disponíveis, como o HTTPotion e o Mint. Estas bibliotecas oferecem diferentes recursos e podem ser mais adequadas para diferentes casos de uso. Portanto, é sempre bom avaliar as diferentes opções antes de escolher qual utilizar em seu projeto.

Além disso, é importante lembrar que ao enviar uma solicitação HTTP, você também pode receber uma resposta em diferentes formatos, como JSON, XML ou HTML. Portanto, é necessário estar ciente do formato da resposta e fazer o tratamento adequado em seu código para lidar com essas diferentes possibilidades.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como enviar solicitações HTTP em Elixir:

- Documentação HTTPoison: https://hexdocs.pm/httpoison
- Documentação HTTPotion: https://hexdocs.pm/httpotion
- Documentação Mint: https://hexdocs.pm/mint/index.html