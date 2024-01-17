---
title:                "Juntando strings"
html_title:           "Haskell: Juntando strings"
simple_title:         "Juntando strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que & Porquê?

Concatenar strings é o ato de unir duas ou mais strings em uma única string. É uma operação útil para combinar informações e criar novas strings a partir de strings existentes. Programadores geralmente realizam a concatenação de strings para criar saídas de texto mais complexas ou alterar dinamicamente strings em programas.

## Como fazer:

**Exemplo 1: Concatenando duas strings**

```Haskell
nome = "João"
sobrenome = "Silva"
nomeCompleto = nome ++ " " ++ sobrenome
```
Output:
```
"João Silva"
```

**Exemplo 2: Concatenando strings e variáveis**
```Haskell
idade = 25
mensagem = "Eu tenho " ++ show idade ++ " anos."
```
Output:
```
"Eu tenho 25 anos."
```

## Mergulho Profundo:

**Contexto histórico**

A concatenação de strings é uma operação comum em programação, usada desde os primeiros dias da linguagem de programação Haskell. Foi introduzida e popularizada pela linguagem de programação Lisp em 1958.

**Alternativas**

Em Haskell, a concatenação de strings com o operador `` `, entre outros métodos. No entanto, a concatenação de strings com o operador `++` é geralmente mais eficiente.

**Detalhes de implementação**

Internamente, a operação de concatenação de strings em Haskell é implementada como uma função simples que percorre as strings e as une uma a uma. Isso é feito de forma eficiente, com complexidade de tempo linear em relação ao tamanho das strings.

## Veja também:

- [Documentação oficial do Haskell sobre strings](http://haskell.org/onlinereport/standard-prelude.html#t%3AString)
- [Outras operações úteis com strings em Haskell](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Introdução à linguagem Haskell](https://wiki.haskell.org/Learn_Haskell_in_10_minutes)