---
title:                "C: Imprimindo saída de depuração"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é útil?

Quando estamos escrevendo um programa em C, é comum nos depararmos com erros ou comportamentos inesperados. Nesses momentos, imprimir saída de depuração pode ser extremamente útil para descobrir onde está o erro e corrigi-lo. Além disso, a saída de depuração também pode ser usada para entender melhor como o programa está funcionando e como cada variável está sendo alterada ao longo do código.

## Como fazer a impressão de saída de depuração em C?

Para imprimir saída de depuração em C, usamos a função `printf()` da biblioteca padrão `stdio.h`. Essa função recebe uma string de formato que pode conter placeholders para variáveis, e então imprime o valor dessas variáveis na saída. Vamos ver um exemplo:

```C
#include <stdio.h>

int main() {

  int numero = 10;
  printf("O valor da variável numero é %d\n", numero);

  return 0;
}
```

A saída desse código será:

```
O valor da variável numero é 10
```

Podemos usar vários placeholders para imprimir diferentes tipos de variáveis, por exemplo `%d` para inteiros, `%f` para floats, `%c` para caracteres e `%s` para strings. É importante respeitar a ordem dos argumentos da função `printf()` para que os valores sejam impressos corretamente.

## Mergulho Profundo na impressão de saída de depuração

Além de simplesmente imprimir valores de variáveis, podemos usar a saída de depuração para entender melhor o fluxo do programa. Podemos utilizar `printf()` em diferentes partes do código para imprimir o valor de uma variável em momentos específicos, o que pode nos ajudar a identificar onde o programa está parando ou qual o valor de uma variável em determinado ponto do código.

Outro recurso interessante é a possibilidade de imprimir valores em diferentes bases numéricas. Por exemplo, podemos imprimir um número em binário usando o placeholder `%b` ou em hexadecimal usando `%x`.

Além disso, existem bibliotecas de terceiros que permitem uma impressão de saída de depuração ainda mais poderosa, com cores e informações adicionais, como a biblioteca `dbg.h`.

## Veja também

- [Documentação oficial do printf() em C](https://en.cppreference.com/w/c/io/printf)
- [Tutorial sobre debug em C](https://www.programiz.com/c-programming/debugging)
- [Tutorial sobre a biblioteca dbg.h](https://c.learncodethehardway.org/book/ex20.html)