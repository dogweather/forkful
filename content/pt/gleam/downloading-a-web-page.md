---
title:                "Baixando uma página da web"
html_title:           "Gleam: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que Baixar uma Página da Web?

Baixar uma página da web pode ser útil para uma variedade de propósitos, como salvar conteúdo offline, realizar análises de dados ou automatizar tarefas de coleta de informações.

## Como Fazer

Existem várias maneiras de baixar uma página da web usando Gleam. Aqui está um exemplo simples, utilizando a biblioteca padrão da linguagem:

```
Gleam.Ports.Http.get("https://www.example.com")
|> Gleam.IO.to_string
|> Gleam.IO.write_file("example.html")
```

Este código faz uma requisição HTTP para a URL especificada e salva o conteúdo da página em um arquivo chamado "example.html". Ao executar este código, você terá baixado com sucesso a página da web especificada.

## Mergulho Profundo

Além do exemplo acima, existem outras opções para baixar páginas da web usando Gleam. Uma delas é a biblioteca de terceiros chamada "gleam-scrape", que oferece funcionalidades mais avançadas para extrair informações específicas de uma página.

Existem também outras opções disponíveis além da biblioteca padrão, como a biblioteca "gleam-curl", que permite fazer chamadas de sistema utilizando o programa curl. Isso pode ser útil se você precisar de mais controle sobre as configurações da requisição HTTP.

Além disso, existem várias técnicas e práticas recomendadas para lidar com problemas comuns ao baixar páginas da web, como lidar com erros de conexão ou timeout. Você pode se aprofundar mais nesses tópicos com a documentação oficial da linguagem e as comunidades de usuários.

## Veja Também

- Documentação oficial do Gleam: https://gleam.run/
- Biblioteca gleam-scrape: https://github.com/gleam-lang/gleam-scrape
- Biblioteca gleam-curl: https://github.com/pragdave/gleam-curl