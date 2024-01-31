---
title:                "Exibindo saídas de depuração"
date:                  2024-01-20T17:51:57.377263-07:00
model:                 gpt-4-1106-preview
simple_title:         "Exibindo saídas de depuração"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Imprimir saídas de depuração significa jogar mensagens na tela para entender o que tá rolando no código. Fazemos isso geralmente para pegar bugs ou ver o fluxo do programa.

## Como Fazer:
```C
#include <stdio.h>

int main() {
    // Exemplo de depuração com printf
    int x = 10;
    printf("O valor de x é: %d\n", x);

    // Condicionais também ajudam na hora de debugar
    if(x == 10) {
        printf("Debug: X encostou no valor mágico 10!\n");
    }

    return 0;
}
```

Saída:
```
O valor de x é: 10
Debug: X encostou no valor mágico 10!
```

## Aprofundando:
Historicamente, printf é o cavalo de batalha da depuração em C, surgindo com a linguagem nos anos 70. Alternativas incluem as poderosas funções de debug do gdb ou até mesmo escrever logs para um arquivo. Quanto à implementação, printf pode ser menos performática comparada a métodos como escrever direto no arquivo de descrição de fd (file descriptor).

## Confira Também:
- [GNU GDB (GNU Project Debugger)](https://www.gnu.org/software/gdb/)
- Manuais da função printf: `man 3 printf` no terminal UNIX/Linux
- Livro "C Programming Language" de Brian Kernighan e Dennis Ritchie, especialmente a seção sobre E/S padrão.
