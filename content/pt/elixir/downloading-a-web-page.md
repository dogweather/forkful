---
title:                "Baixando uma página da web."
html_title:           "Elixir: Baixando uma página da web."
simple_title:         "Baixando uma página da web."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

##Por que Baixar uma Página da Web com Elixir é uma Boa Idéia?

Se você está trabalhando em um projeto que requer a coleta de dados de uma página da web, usar Elixir pode ser uma ótima opção. Com sua poderosa biblioteca HTTP e sua capacidade de processar dados em paralelo, é uma linguagem perfeita para tarefas de scraping.

##Como Fazer:

```Elixir
# primeiro, é necessário importar a biblioteca HTTP
iex> http = HTTPoison

# então, usar o método get para fazer a requisição para a página desejada
iex> response = http.get("https://www.example.com")

# usar o comando status_code para verificar se a requisição foi bem sucedida (200 indica sucesso)
iex> response.status_code 
200

# usar o comando body para acessar o conteúdo da página em formato HTML
iex> response.body 
"<html><head>...</head><body><h1>Bem-vindo ao Elixir!</h1>...</body></html>"
```

Com esses comandos básicos, já é possível baixar o conteúdo de uma página da web em HTML. A partir daqui, é possível processar e extrair os dados desejados usando ferramentas de parsing ou regex.

##Aprofundando-se:

Além dos comandos básicos de requisição e obtenção de conteúdo, a biblioteca HTTPoison também oferece diversas funcionalidades avançadas, como suporte a proxy, cabeçalhos personalizáveis e autenticação. Além disso, é possível encontrar outras bibliotecas especializadas em scraping, como o mochiweb e o hackney.

Caso o conteúdo da página esteja em formato JSON, é possível usar a biblioteca Poison para converter o conteúdo para um formato manipulável por Elixir. Outra opção é usar a biblioteca Floki para fazer o parsing do conteúdo em formato HTML e extrair os dados desejados.

##Veja Também:

- [Documentação do HTTPoison](https://github.com/edgurgel/httpoison)
- [Documentação do Poison](https://github.com/devinus/poison)
- [Documentação do Floki](https://github.com/philss/floki)
- [Exemplos de projetos em Elixir com foco em scraping](https://github.com/ayrat555/scrapy)