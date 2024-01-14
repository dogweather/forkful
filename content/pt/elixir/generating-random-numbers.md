---
title:                "Elixir: Geração de números aleatórios"
programming_language: "Elixir"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por que gerar números aleatórios é importante em Elixir?

Gerar números aleatórios é uma parte importante do desenvolvimento em Elixir. Isso porque, ao trabalhar com linguagens funcionais como Elixir, é necessário ter uma compreensão sólida de como gerar e manipular dados aleatórios. Isso é especialmente útil em cenários como jogos, testes automatizados e amostragem de dados.

## Como gerar números aleatórios em Elixir

Gerar números aleatórios em Elixir é muito simples com a ajuda da função `:rand.uniform/0`. Para gerar um número aleatório dentro de um determinado intervalo, você pode usar `:rand.uniform/2` e passar o intervalo como argumento. Vamos ver alguns exemplos de código usando a função `:rand.uniform/0`:

```elixir
# Gera um número aleatório entre 0 e 1
:rand.uniform()

# Gera um número aleatório entre 1 e 100
:rand.uniform(1..100)
```

O resultado dos dois exemplos acima será um número do tipo `float` com até 16 dígitos de precisão. Se você precisar de um número inteiro, pode usar a função `:rand.uniform/1` e especificar o tipo de resultado desejado (`integer` ou `float`). Por exemplo:

```elixir
# Gera um número aleatório inteiro entre 1 e 10
:rand.uniform(1..10, :integer)
```

Há também outras funções úteis para gerar números aleatórios, como `:rand.uniform/1`, `:rand.uniform/2`, `:rand.uniform/3` e `:rand.uniform/4`. Estas funções permitem gerar diferentes tipos de dados aleatórios, como números booleanos, caracteres, atomos e duplas.

## Profundando em números aleatórios em Elixir

Além da função `:rand.uniform/0`, Elixir possui outras funções para geração de números aleatórios, como `:rand.seed/0` para definir uma "semente" para a geração de números e `:rand.seed/1` para definir uma semente específica. Também é possível especificar um gerador de números aleatórios alternativo através da função `:rand.generator/0`.

Outra funcionalidade interessante é a possibilidade de passar a função `:rand.uniform/1` como argumento para outras funções. Isso pode ser útil em cenários onde você precisa gerar dados aleatórios para testar uma função específica, por exemplo.

## Veja também

- Documentação oficial do módulo `:rand` em Elixir: https://hexdocs.pm/elixir/Random.html
- Tutorial de Elixir com exemplos de geração de números aleatórios: https://elixirschool.com/pt/lessons/basics/enum/
- Exemplos práticos de uso da função `:rand.uniform/1` em Elixir: https://simplabs.com/blog/2018/02/28/enum-filter-map.html