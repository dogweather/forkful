---
title:                "Usando um depurador"
date:                  2024-01-26T03:47:47.415344-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando um depurador"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Um depurador é uma ferramenta que permite inspecionar seu código em C enquanto ele é executado, passo a passo, para caçar bugs. Programadores utilizam depuradores para entender como seu código se comporta, corrigir problemas e otimizar desempenho sem jogar um jogo de adivinhação.

## Como fazer:
Digamos que você esteja trabalhando com um programa simples em C que calcula o fatorial de um número, mas há um glitch. Para usar um depurador como `gdb` (GNU Debugger), primeiramente compile com a bandeira `-g` para incluir informações de depuração:

```c
// compile com: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // Uma simples verificação para entrada negativa
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("O fatorial de %d é %ld\n", number, result);
    return 0;
}
```

Em seguida, execute-o no gdb:

```shell
$ gdb ./factorial
```

Defina um ponto de interrupção na função `factorial` e execute o programa:

```gdb
(gdb) break factorial
(gdb) run
```

Quando atingir o ponto de interrupção, avance linha por linha usando `next` ou `n` e inspecione variáveis com `print` ou `p`:

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

A saída de amostra fornecerá valores em tempo real e o fluxo de execução do programa.

## Aprofundamento
Depuradores existem desde a década de 1960, evoluindo de simples monitores para aplicações complexas baseadas em GUI. A depuração baseada em impressão era comum antes do desenvolvimento de depuradores maduros. Alternativas ao `gdb` incluem `lldb`, `dbx` ou depuradores integrados a IDEs como aqueles no Visual Studio ou CLion.

Ao lidar com depuradores, a implementação varia — alguns podem capturar erros de tempo de execução, examinar a memória ou até reverter a execução de um programa. O `gdb` pode se anexar a processos em execução, permitindo a depuração de software já em execução, uma vantagem para corrigir bugs de sistemas ao vivo.

## Veja também
- GNU Debugger (GDB): https://www.gnu.org/software/gdb/documentation/
- Debugging with GDB: https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB Debugger: https://lldb.llvm.org/use/tutorial.html
- Técnicas de Depuração em C: http://www.cprogramming.com/debugging/debugging.html
