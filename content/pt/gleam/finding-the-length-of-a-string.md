---
title:    "Gleam: Encontrando o comprimento de uma string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma habilidade importante em programação que pode ser usada para diversas finalidades, como validar entradas de usuário ou manipular dados. Além disso, entender como encontrar o comprimento de uma string pode ajudar a compreender melhor outros conceitos de linguagem de programação, como arrays e loops.

## Como fazer

Para encontrar o comprimento de uma string em Gleam, podemos usar a função `String.length()` que retorna o número de caracteres em uma determinada string. Vamos ver alguns exemplos de código:

```
// Declara uma variável string
let texto = "Olá, mundo!"

// Encontra o comprimento da string e armazena em uma variável
let tamanho = String.length(texto)

// Imprime o resultado
IO.print("O comprimento da string é " <> String.to_int(tamanho))
```

O código acima vai imprimir "O comprimento da string é 11" no terminal. Podemos também usar a função `IO.inspect()` para imprimir diretamente o resultado sem precisar armazenar em uma variável:

```
let texto = "Olá, mundo!"

IO.inspect(String.length(texto))
```

Este código vai imprimir o número 11 no terminal.

## Mergulho profundo

Uma coisa importante a se notar é que a função `String.length()` conta o número de caracteres, incluindo espaços em branco e pontuação. Isso significa que o comprimento de uma string pode ser maior do que o número de palavras que ela contém. Além disso, vale ressaltar que o comprimento de uma string é determinado pela sua codificação de caracteres. Por exemplo, em UTF-8 um caractere especial como "ç" é considerado como um único caractere, enquanto em UTF-16 ele pode ser contado como dois caracteres.

## Veja também

- Documentação oficial do Gleam: [https://gleam.run/documentation/#string-length](https://gleam.run/documentation/#string-length)
- Tutorial sobre trabalhar com strings em Gleam: [https://learnxinyminutes.com/docs/gleam/](https://learnxinyminutes.com/docs/gleam/)