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

--------------------------------------------------
## O que e Porque?

Imprimir a saída de depuração é uma técnica frequentemente usada por programadores para visualizar o estado do código durante a execução. Isso é útil para identificar e corrigir erros, bem como para entender melhor o comportamento do programa.

## Como fazer:

As impressões de depuração no Gleam podem ser feitas usando a macro `debug!` seguida de uma expressão a ser impressa. Por exemplo:

```Gleam
debug! 2 + 3
```

Isso imprimirá `5` no console durante a execução do programa.

Você também pode adicionar uma mensagem à sua impressão, usando a forma `debug!("Sua mensagem", expressão)`. Por exemplo:

```Gleam
debug!("O resultado é:", 2 + 3)
```

A saída seria `O resultado é: 5`.

## Aprofundando:

Imprimir a saída de depuração não é exclusivo do Gleam, mas é uma técnica amplamente utilizada em programação. Está disponível em várias linguagens de programação e é uma das maneiras mais rápidas e eficazes de rastrear bugs em um programa.

Algumas alternativas para imprimir a saída de depuração incluem o uso de um depurador, como o GDB ou o LLDB, ou a utilização de ferramentas de rastreamento de chamadas, como o strace.

As impressões de depuração no Gleam são implementadas usando macros, que permitem que o código seja gerado dinamicamente em tempo de compilação. Isso torna as impressões de depuração muito eficientes e não afeta o desempenho do programa em tempo de execução.

## Veja também:

- [Documentação do Gleam sobre impressões de depuração](https://gleam.run/book/concurrency-debugging)
- [Artigo sobre impressões de depuração em Rust](https://docs.rs/log/0.4.8/log/)
- [Tutoriais sobre o depurador GDB](https://www.gnu.org/software/gdb/documentation/)