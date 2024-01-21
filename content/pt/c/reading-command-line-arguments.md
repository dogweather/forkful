---
title:                "Lendo argumentos da linha de comando"
date:                  2024-01-20T17:55:36.776525-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Ler argumentos da linha de comando permite que programas em C recebam entradas externas quando iniciam. Programadores fazem isso para permitir a personalização do comportamento do programa sem alterar o código-fonte.

## Como Fazer:
```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    if (argc > 1) {
        printf("Olá! Você passou %d argumento(s) através da linha de comando.\n", argc - 1);
        for (int i = 1; i < argc; i++) {
            printf("Argumento %d: %s\n", i, argv[i]);
        }
    } else {
        printf("Olá! Você não passou argumentos.\n");
    }
    return 0;
}
```

Saída de exemplo quando nenhum argumento é passado:
```
Olá! Você não passou argumentos.
```

Saída de exemplo com argumentos:
```
Olá! Você passou 2 argumento(s) através da linha de comando.
Argumento 1: Olá
Argumento 2: Mundo
```

## Mergulho Profundo
Historicamente, argumentos da linha de comando datam do tempo dos primeiros sistemas operacionais, quando a interface de linha de comando era o principal método de interação com o computador. Hoje, existem alternativas como interfaces gráficas e APIs, mas para muitos usos, a prática ainda é essencial, principalmente para ferramentas de desenvolvimento e scripts.

Em C, essas informações são acessadas pelo `argc` e `argv`. O `argc` representa o número de argumentos, e `argv` é um array de strings com os argumentos propriamente ditos. `argv[0]` é reservado para o nome do programa, então os argumentos reais começam em `argv[1]`. Detalhes de implementação são importantes, como a garantia que `argv[argc]` é `NULL`, o que pode ser útil para saber quando você alcançou o fim dos argumentos.

## Veja Também
- [GNU C Library: Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)
- [Wikipedia: Command-line interface](https://en.wikipedia.org/wiki/Command-line_interface#Arguments)