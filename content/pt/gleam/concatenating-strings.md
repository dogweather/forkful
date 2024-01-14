---
title:                "Gleam: Juntando cadeias de caracteres"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que concatenar strings é útil em Gleam?

Concatenar, ou juntar, strings é uma tarefa comum em programação. Em Gleam, a concatenação de strings é especialmente útil para criar mensagens personalizadas, gerar relatórios ou formatar dados de saída. É uma ferramenta fundamental para criar scripts e programas dinâmicos e interativos.

## Como fazer a concatenação de strings em Gleam

Fazer a concatenação de strings em Gleam é simples. Basta utilizar o operador "+" para juntar duas ou mais strings. Veja um exemplo:

```Gleam
let nome = "Maria"
let sobrenome = "Silva"
let nome_completo = nome + " " + sobrenome
```

O valor da variável "nome_completo" será "Maria Silva". É possível também adicionar strings diretamente a uma variável existente, utilizando o operador "+=":

```Gleam
let mensagem = "Olá "
mensagem += nome_completo
mensagem += "! Bem-vinda ao meu blog!"
```

Agora, o valor da variável "mensagem" será "Olá Maria Silva! Bem-vinda ao meu blog!". Perceba como a concatenação de strings permite criar mensagens personalizadas e dinâmicas.

## Detalhando a concatenação de strings em Gleam

Em Gleam, a concatenação de strings é feita através do operador "+". Porém, é importante lembrar que esse operador serve apenas para unir strings, não sendo possível realizar operações matemáticas entre elas. Além disso, é possível concatenar strings com outros tipos de dados, como inteiros e booleanos, mas é preciso convertê-los para string antes.

Outro detalhe importante é que a concatenação de strings em Gleam é feita de forma eficiente e segura, evitando problemas comuns como a falta de memória. Assim, é possível utilizar essa funcionalidade sem se preocupar com possíveis erros ou bugs.

## Veja também

- [Documentação oficial sobre strings em Gleam](https://gleam.run/stdlib/string.html)
- [Tutorial sobre manipulação de strings em Gleam](https://dev.to/t/hacky-dev-gleam)

Esperamos que este artigo tenha sido útil para entender melhor a concatenação de strings em Gleam. Com essa ferramenta, você poderá criar programas mais dinâmicos e interativos. Continue explorando a linguagem e veja o que mais é possível fazer!