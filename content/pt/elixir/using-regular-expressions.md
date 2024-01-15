---
title:                "Usando Expressões Regulares"
html_title:           "Elixir: Usando Expressões Regulares"
simple_title:         "Usando Expressões Regulares"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Elixir?

Expressões regulares são uma ferramenta poderosa para manipulação de texto em qualquer linguagem de programação, incluindo o Elixir. Com elas, é possível encontrar, extrair, substituir e manipular padrões de texto de forma eficiente e concisa.

## Como usar expressões regulares em Elixir

Para utilizar expressões regulares em Elixir, é necessário utilizar o módulo `Regex`, que contém funções para criar, manipular e fazer correspondência com expressões regulares.

Um exemplo simples é encontrar todas as ocorrências de números em uma string:

```
Elixir
str = "Tenho 10 maçãs e 5 laranjas."
Regex.scan(~r/\d+/, str)
# Saída: ["10", "5"]
```

Também é possível utilizar a função `Regex.run/2` para encontrar apenas a primeira correspondência:

```
Elixir
str = "Tenho 10 maçãs e 5 laranjas."
Regex.run(~r/\d+/, str)
# Saída: "10"
```

Para substituir um padrão por outro valor, é possível utilizar a função `Regex.replace/3`:

```
Elixir
str = "Olá, meu nome é João."
Regex.replace(~r/João/, str, "Maria")
# Saída: "Olá, meu nome é Maria."
```

Para mais exemplos e informações sobre as funções do módulo `Regex`, consulte a documentação oficial.

## Aprofundando-se em expressões regulares

Apesar de serem altamente úteis, expressões regulares podem ser complexas e confusas para iniciantes. É fundamental entender a sintaxe e as convenções utilizadas para criar padrões de texto, além de conhecer as diversas opções de modificadores e metacaracteres disponíveis.

Além disso, é importante entender quando usar expressões regulares, pois nem sempre elas são a melhor solução para manipulação de texto. Outras opções, como funções de string e listas, podem ser mais adequadas em alguns casos.

## Veja também

- [Documentação oficial do módulo Regex](https://hexdocs.pm/elixir/Regex.html)
- [Cheatsheet de expressões regulares em Elixir](https://gist.github.com/AdrianaUrrea/8553766)
- [Artigo sobre quando não usar expressões regulares em Elixir](https://lucianopessoa.medium.com/when-not-to-use-regular-expressions-in-elixir-1fe48624fefb)