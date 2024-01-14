---
title:                "Elixir: Buscando e substituindo texto"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Neste artigo, vamos explorar o poder da substituição de texto em Elixir. Aprenderemos por que é importante saber esta habilidade e como utilizá-la em nossos projetos.

## Como Fazer
Em Elixir, a substituição de texto é geralmente feita através da função `String.replace/4`. Esta função aceita quatro parâmetros: a string de entrada, o padrão a ser procurado, o padrão de substituição e as opções de busca. Vamos ver alguns exemplos:

```
iex> String.replace("Olá, mundo!", "mundo", "Elixir")
"Olá, Elixir!"
```

No exemplo acima, usamos a função `String.replace/4` para substituir a palavra "mundo" por "Elixir" na string "Olá, mundo!". O resultado é a nova string "Olá, Elixir!".

Além da substituição simples, também podemos usar regex para encontrar e substituir padrões em uma string:

```
iex> String.replace("Elixir é muito legal", ~r/muito/, "incrível")
"Elixir é incrível legal"
```

A função `String.replace/4` também aceita opções de busca, como ignorar letras maiúsculas ou minúsculas, substituição global e substituição apenas da primeira ocorrência. Para obter mais detalhes sobre as opções disponíveis, consulte a documentação de `String.replace/4`.

## Mergulho Profundo
Em alguns casos, pode ser necessário realizar substituições mais avançadas em strings. Nesses casos, podemos usar a função `Regex.replace/3` que nos permite usar expressões regulares para encontrar e substituir padrões em uma string.

```
iex> Regex.replace("2007.04.05", ~r/\./, "/", global: true)
"2007/04/05"
```

Neste exemplo, usamos regex para substituir todos os pontos por barras na data "2007.04.05". Podemos também passar uma função como terceiro parâmetro para `Regex.replace/3`, permitindo ainda mais flexibilidade na substituição de padrões em uma string.

## Veja Também
- [Documentação oficial de String](https://hexdocs.pm/elixir/String.html)
- [Documentação oficial de Regex](https://hexdocs.pm/elixir/Regex.html)
- [Guia rápido de Regex em Elixir](https://dev.to/leperf/a-quick-and-dirty-regex-introduction-for-elixir-software-engineers-mnl)