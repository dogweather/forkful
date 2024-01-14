---
title:    "Elixir: Usando expressões regulares"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Por que usar Expressões Regulares em Elixir?

Expressões Regulares (também conhecidas como RegEx) são uma ferramenta poderosa e versátil para manipulação de strings. Se você lida com dados textuais em sua programação, usar RegEx pode economizar tempo e tornar seu código mais eficiente.

## Como utilizar Expressões Regulares em Elixir

Em Elixir, para utilizar Expressões Regulares, primeiro é preciso importar o módulo Regex:

```
import Regex
```

Em seguida, podemos usar a função `match?` para verificar se um padrão de RegEx é encontrado em uma string:

```
match?("Olá, mundo!", ~r/mundo/) # retorna true
```

Também é possível extrair partes de uma string que correspondem a um padrão específico usando a função `scan`:

```
scan("A linguagem de programação Elixir é incrível", ~r/\w+/) # retorna ["A", "linguagem", "de", "programação", "Elixir", "é", "incrível"]
```

## Explorando mais a fundo as Expressões Regulares

As Expressões Regulares em Elixir são baseadas na sintaxe do módulo [PCRE](https://hexdocs.pm/elixir/Regex.html#content). Algumas funcionalidades avançadas incluem o uso de grupos de captura, substituições e retrovisor.

Além disso, é possível utilizar os operadores `=~` e `!~` para comparar uma string com um padrão de RegEx:

```
"Elixir é incrível" =~ ~r/incrível/ # retorna true
"Expressões Regulares são complicadas" =~ ~r/compl./ # retorna true
```

## Veja também

- [Documentação oficial do módulo Regex em Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Tutorial de Expressões Regulares em Elixir](https://www.tutorialspoint.com/elixir/elixir_regular_expressions.htm)
- [Guia de Expressões Regulares em PCRE](https://www.rexegg.com/regex-quickstart.html)