---
title:                "Elixir: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que

Ao programar em Elixir, muitas vezes encontramos situações em que precisamos manipular strings e caracteres. Às vezes, essas strings contêm caracteres que não são necessários para a nossa lógica de programação e precisam ser removidos. Aprender a deletar caracteres que correspondem a um determinado padrão pode tornar o nosso código mais eficiente e legível.

## Como Fazer

Para deletar caracteres que correspondem a um padrão em uma string, podemos usar a função `String.replace/3` em combinação com expressões regulares. Por exemplo, se quisermos remover todos os caracteres numéricos de uma string, podemos fazer o seguinte:

```Elixir
string = "Elixir123"
String.replace(string, ~r/[0-9]/, "")
```
A saída seria `"Elixir"` sem os números.

Podemos até mesmo usar `String.replace/3` para substituir esses caracteres por algo diferente. Por exemplo, se quisermos substituir todos os caracteres maiúsculos por asteriscos, podemos fazer o seguinte:

```Elixir
string = "Elixir Rocks"
String.replace(string, ~r/[A-Z]/, "*")
```
A saída seria `"*lixir *ocks"`.

Esses são apenas dois exemplos simples de como podemos usar a função `String.replace/3` para deletar caracteres correspondentes a um padrão em uma string. As possibilidades são infinitas e dependem da nossa criatividade e necessidade.

## Profundidade

Ao usar a função `String.replace/3` com expressões regulares, podemos usar várias opções para tornar nossa lógica de remoção de caracteres mais precisa. Alguns exemplos incluem:

- `^` para coincidir apenas no início da string
- `$` para coincidir apenas no final da string
- `+` para combinar um ou mais caracteres
- `?` para combinar zero ou um caractere

Podemos até mesmo criar nossas próprias funções para lidar com casos específicos em que `String.replace/3` pode não ser suficiente.

Para mais informações sobre expressões regulares e todas as opções que podemos usar ao lidar com elas, consulte a documentação oficial do Elixir.

## Veja Também

- Documentação oficial do Elixir sobre expressões regulares: https://hexdocs.pm/elixir/Regex.html
- Um guia passo-a-passo sobre expressões regulares em Elixir: https://elixirschool.com/blog/regex-in-elixir/