---
title:                "Escrevendo no erro padrão"
html_title:           "Gleam: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O que é e por quê?

Escrever para o erro padrão é uma técnica comum usada por programadores para imprimir mensagens de erro ou depuração durante a execução de um código. Isso permite que os desenvolvedores identifiquem e resolvam problemas em seu código de forma mais eficiente.

## Como fazer:

Para escrever para o erro padrão em Gleam, utilize a função "error!" seguida de uma string contendo a mensagem que deseja imprimir. Veja um exemplo:

```Gleam
import gleam/io

let mensagem = "Essa é uma mensagem de erro!"

error!(mensagem)
```

A saída desse código seria:

```
Essa é uma mensagem de erro!
```
## Deep Dive:

Escrever para o erro padrão é uma prática antiga, presente em diversas linguagens de programação. Ela surgiu como uma forma de fornecer informações úteis aos programadores durante a execução de um código, ajudando-os a encontrar e corrigir erros.

Uma alternativa ao uso do erro padrão é o uso de registros de log, que servem para armazenar informações sobre a execução do código. No entanto, escrever para o erro padrão é mais simples e rápido, e pode ser usado em conjunto com registros de log para uma melhor detecção de problemas.

Em Gleam, a função "error!" é implementada utilizando a função "sys::printf" da biblioteca padrão, que permite imprimir uma string formatada para o erro padrão.

## Veja também:

- [Função "error!" na documentação oficial de Gleam](https://gleam.run/documentation/#error!)