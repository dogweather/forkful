---
title:    "Elm: Concatenando strings"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por que

Ao programar em Elm, muitas vezes é necessário combinar strings ou textos para formar uma única string. Isso pode ser útil para criar mensagens, nomes dinâmicos ou para exibir dados formatados. Neste artigo, vamos explorar como concatenar strings em Elm e como isso pode ser benéfico para os programadores.

## Como Fazer

Em Elm, a concatenação de strings é feita usando o operador de adição `++`. Vamos ver alguns exemplos de como usar esse operador para combinar diferentes strings.

```
Elm
"Olá" ++ " " ++ "mundo" --> "Olá mundo"
```

Note que podemos também adicionar espaços entre as strings para criar uma concatenação mais legível.

Podemos também usar este operador para combinar variáveis ou valores com strings.

```
Elm
let nome = "João"
let sobrenome = "Silva"
nome ++ " " ++ sobrenome --> "João Silva"
```

Além disso, para adicionar um valor numérico a uma string, é necessário primeiro convertê-lo para uma string usando a função `toString`.

```
Elm
let idade = 25
"Mariana tem " ++ toString idade ++ " anos" --> "Mariana tem 25 anos"
```

## Aprofundando

É importante lembrar que, em Elm, strings são imutáveis, ou seja, não podem ser alteradas depois de criadas. Portanto, a concatenação de strings cria uma nova string a partir das strings existentes. Isso garante a integridade dos dados e evita efeitos colaterais indesejados.

Também é possível concatenar várias strings usando a função `concat`.

```
Elm
concat ["Este", " ", "é", " ", "um", " ", "exemplo"] --> "Este é um exemplo"
```

Além disso, é possível usar o operador `++` com outras estruturas de dados, como listas e outros tipos definidos pelo usuário.

## Veja também

- [Documentação oficial sobre strings em Elm](https://elm-lang.org/docs/strings)
- [Elm School: concatenando strings](https://elmprogramming.com/concatenating-strings.html)
- [Little program: concatenação em Elm](https://littleprogram.arabelladz.org/elm/string-concatext?a=1)