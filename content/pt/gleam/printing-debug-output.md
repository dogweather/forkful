---
title:                "Imprimindo saída de depuração"
html_title:           "Gleam: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

O uso de impressão de saída para debug é uma ferramenta essencial para os programadores que desejam rastrear o comportamento de seus programas e encontrar possíveis erros de maneira eficiente.

## Como Fazer

Para utilizar a impressão de saída para debug em Gleam, você pode seguir o exemplo abaixo:

```Gleam
import gleam/io

pub fn main() {
  let name = "Maria"
  let age = 28

  io.print("Nome: {}", [name])
  io.print("Idade: {}", [age])
}
```

Este código irá imprimir a seguinte saída:

```
Nome: Maria
Idade: 28
```

Note que utilizamos `{}` nos textos que queríamos inserir o valor das variáveis. Isso é chamado de formatação de string e é uma maneira conveniente de exibir o valor de variáveis em textos.

## Deep Dive

Existem diferentes métodos de impressão de saída para debug em Gleam, como `io.print`, `io.print_line`, `io.print_info`, entre outros. Cada um destes métodos pode ser usado para imprimir diferentes tipos de dados, como números, texto, listas e até mesmo estruturas de dados complexas.

Além disso, a impressão de saída para debug pode ser combinada com estruturas de controle de fluxo, como `if`, `case` e `for`, para exibir informações apenas quando certas condições são atendidas.

Não subestime o poder da impressão de saída para debug em sua jornada de programação. Com ela, você pode economizar tempo e esforço na hora de encontrar e corrigir erros em seu código.

## Veja Também

- [Documentação Oficial do Gleam](https://gleam.run/documentation/)
- [Tutorial de Gleam](https://gleam.run/getting-started/)
- [Gleam no Github](https://github.com/gleam-lang/gleam)