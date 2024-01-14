---
title:                "C: Exibindo a saída de depuração"
simple_title:         "Exibindo a saída de depuração"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que
A impressão de saída de depuração é uma ferramenta essencial para programadores em C. Permite que verifiquemos o comportamento do nosso código e encontremos erros, o que economiza tempo e esforço no processo de depuração.

## Como fazer
Aqui estão alguns exemplos de como imprimir saída de depuração em C:

```C
#include <stdio.h>

int main()
{
    int x = 5;
    printf("O valor de x é: %d\n", x);
    // saída: O valor de x é: 5

    float y = 3.14;
    printf("O valor de y é: %.2f\n", y);
    // saída: O valor de y é: 3.14

    char z = 'a';
    printf("O valor de z é: %c\n", z);
    // saída: O valor de z é: a

    return 0;
}
```

Podemos usar a função `printf()` para imprimir diferentes tipos de dados, como inteiros, flutuantes e caracteres. O formato da saída depende dos caracteres de formato que usamos. Por exemplo, `%d` é usado para inteiros e `%f` é usado para flutuantes.

## Aprofundando
Além de imprimir valores de variáveis, a saída de depuração também pode ser útil para acompanhar o fluxo do código. Podemos incluir mensagens de texto para nos ajudar a entender o que está acontecendo em diferentes partes do programa. Por exemplo:

```C
#include <stdio.h>

int main()
{
    int x = 3;

    if (x > 5)
    {
        printf("x é maior que 5\n");
    }
    else
    {
        printf("x é menor ou igual a 5\n");
    }
    // saída: x é menor ou igual a 5

    return 0;
}
```

Podemos usar a saída de depuração para verificar se nossos loops estão funcionando corretamente, quantas vezes eles são executados e quais valores são atribuídos às variáveis em cada iteração. Isso pode ser especialmente útil quando estamos tentando encontrar erros em loops complexos.

## Veja também
Aqui estão alguns recursos adicionais sobre impressão de saída de depuração em C:

[Guia de Iniciantes para Imprimir Saída de Depuração em C](https://www.cs.cmu.edu/~guna/15-123S11/Lectures/Lecture24.pdf)
[Debugging with printf() in C](https://www.electronicdesign.com/technologies/embedded-revolution/article/21798302/debugging-with-printf-in-c)
[C Programming Tutorial: Debugging with printf()](https://youtu.be/dXgJzZ4Lx6c)