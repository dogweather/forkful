---
title:                "Elixir: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que usar Elixir para baixar páginas da web?

Se você está procurando uma linguagem de programação que seja poderosa, escalável e com uma sintaxe elegante, então Elixir é a escolha certa. Usando Elixir, você pode facilmente baixar páginas da web e manipular os dados obtidos de maneira eficiente. Neste artigo, vamos explorar como fazer isso passo a passo.

## Como Fazer

Para começar, você precisa ter o Elixir instalado em seu computador. Em seguida, crie um novo arquivo com a extensão .exs (para scripts Elixir) e vamos iniciar nosso código! Vamos precisar de uma biblioteca chamada `HTTPoison` para fazer as solicitações HTTP necessárias e obter os dados da página da web. Você pode instalá-la com o comando `mix deps.get`.

Dentro do seu arquivo .exs, importe o módulo `HTTPoison` e a função `Application.ensure_all_started` para garantir que todas as dependências necessárias estejam carregadas. Em seguida, use a função `HTTPoison.get` para fazer uma solicitação HTTP para a página da web desejada e armazenar a resposta em uma variável. Por exemplo:

```Elixir
HTTPoison.get("https://www.exemplo.com.br")
|> case do
    {:ok, %HTTPoison.Response{body: body}} ->
        # Aqui temos acesso aos dados da página armazenados na variável "body"
    {:error, %HTTPoison.Error{reason: reason}} ->
        # Tratamento de erro
end
```

Agora, você pode manipular esses dados da maneira que desejar, como extrair informações específicas ou analisar o HTML da página. Para isso, você pode usar uma biblioteca chamada `Floki`, que oferece recursos úteis para trabalhar com HTML. Não é necessário instalá-la, pois ela já vem com o Elixir.

## Deep Dive

Para entendermos melhor como o processo de baixar páginas da web funciona em Elixir, aqui está uma visão geral do que acontece por trás das cenas:

- A biblioteca `HTTPoison` usa a biblioteca `gun` para fazer as solicitações HTTP.
- Quando uma solicitação é feita, `gun` envia uma conexão TCP para o servidor da página.
- O servidor responde com os dados da página e `gun` os envia de volta para `HTTPoison`.
- `HTTPoison` verifica o status da resposta e a retorna em forma de tupla, onde a primeira parte é o status e a segunda é um mapa contendo os dados da resposta, como o corpo da página e os cabeçalhos.

## Veja Também

- [Documentação da biblioteca HTTPoison](https://hexdocs.pm/httpoison)
- [Documentação da biblioteca Floki](https://hexdocs.pm/floki)
- [Tutorial para iniciantes em Elixir](https://elixir-lang.org/getting-started/introduction.html)