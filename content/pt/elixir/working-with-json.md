---
title:                "Trabalhando com json"
html_title:           "Elixir: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Trabalhar com JSON é uma tarefa comum para programadores que lidam com dados em suas aplicações. JSON, ou JavaScript Object Notation, é um formato leve e fácil de ler e escrever para trocar informações entre sistemas. Programadores usam JSON para transmitir dados entre um cliente e um servidor, ou para armazenar dados em arquivos.

## Como Fazer:

Elixir possui uma biblioteca padrão chamada `Jason` que nos permite trabalhar facilmente com JSON. Para converter um mapa em JSON, podemos usar a função `encode!`:

```Elixir
data = %{nome: "João", idade: 25}
Jason.encode!(data)
```
Isso nos retorna uma string no formato JSON:

`"{\"nome\":\"João\",\"idade\":25}"`

Para converter uma string JSON de volta para um mapa, usamos a função `decode!`:

```Elixir
json = "{\"nome\":\"João\",\"idade\":25}"
Jason.decode!(json)
```

Isso nos retorna o mapa original:

`%{"nome" => "João", "idade" => 25}`

## Aprofundando:

JSON foi criado por Douglas Crockford em 2001 e desde então tem se tornado um dos formatos mais populares para trocar informações entre sistemas. Existem outras alternativas como XML e YAML, mas JSON é amplamente utilizado por sua simplicidade e eficiência. Dentro dos sistemas de comunicação, JSON é usado principalmente para criar APIs RESTful.

A biblioteca `Jason` usa o módulo `Poison` para fazer a codificação e decodificação de JSON para Elixir. É possível personalizar as opções de codificação e decodificação através de configurações.

## Veja Também:

- [Documentação da Biblioteca Jason](https://hexdocs.pm/jason/1.2.2/Jason.html)
- [Tutorial de JSON em Elixir](https://www.tutorialspoint.com/elixir/elixir_json.htm)
- [Introdução ao JSON](https://www.json.org/json-pt.html)