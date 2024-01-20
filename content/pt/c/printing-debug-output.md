---
title:                "Imprimindo saída de debug"
html_title:           "C#: Imprimindo saída de debug"
simple_title:         "Imprimindo saída de debug"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

# Imprimir Saída de Depuração em C: Um Guia Informal

## O Que e Por Que?
A impressão de saída de depuração é uma forma de acompanhar o que está acontecendo dentro do seu programa. Programadores a utilizam para identificar e corrigir erros (bugs).

## Como Fazer:
Aqui está como você exibe mensagens de depuração em C:

```C
#include <stdio.h>

int main() {
    int i = 5;
    printf("Debug: i = %d\n", i);  // Saída: Debug: i = 5
    return 0;
}
```
Quando for necessário desativar as mensagens de depuração, basta comentar a linha ou removê-la:

```C
#include <stdio.h>

int main() {
    int i = 5;
    // printf("Debug: i = %d\n", i);
    return 0;
}
```

## Mergulho Profundo
A saída de depuração tem sido usada desde os primeiros dias da programação. Ela é útil, mas tem algumas desvantagens. Uma delas é que nem sempre é prático imprimir mensagens de depuração em um programa em execução.

Temos alternativas para a impressão de depuração, como a utilização de um depurador, onde podemos examinar o estado de um programa passo a passo. No entanto, a facilidade de uso da impressão de depuração faz dela um recurso comumente usado.

A função `printf()` é frequentemente usada para imprimir saída de depuração. Ela é parte da biblioteca padrão C e pode ser usada para imprimir em vários formatos. No exemplo acima, usamos o especificador de formato `%d` para imprimir um número inteiro.

## Veja Também
1. [Documentação da função printf()](https://www.cplusplus.com/reference/cstdio/printf/)
2. [Depuração no Visual Studio](https://docs.microsoft.com/pt-br/visualstudio/debugger/?view=vs-2019)
3. [Depuração no GCC](https://sourceware.org/gdb/current/onlinedocs/gdb/)