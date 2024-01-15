---
title:                "Impressão de saída de depuração"
html_title:           "C++: Impressão de saída de depuração"
simple_title:         "Impressão de saída de depuração"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Às vezes, ao desenvolver um programa em C++, pode ser útil imprimir saídas de depuração para entender melhor como o código está funcionando e encontrar possíveis erros.

## Como fazer

Para imprimir saídas de depuração em C++, usamos a função `cout` da biblioteca padrão `iostream`. Veja um exemplo:

```C++
#include <iostream>

int main() {
    std::cout << "Imprimindo saída de depuração" << std::endl;
    return 0;
}
```

Este código irá imprimir a frase "Imprimindo saída de depuração" na tela. Podemos também imprimir o valor de variáveis usando a mesma função, utilizando o operador de inserção `<<`. Por exemplo:

```C++
int x = 5;
std::cout << "O valor de x é: " << x << std::endl;
```

Este código irá imprimir o valor da variável `x`, que é 5, na tela. Assim, podemos imprimir qualquer tipo de dado que desejamos, incluindo strings, inteiros, floats, entre outros.

## Mergulho profundo

Além da função `cout`, também existem outras formas de imprimir saídas de depuração em C++. Uma delas é a função `cerr`, que é utilizada para imprimir mensagens de erro. Por exemplo:

```C++
#include <iostream>

int main() {
    std::cerr << "Mensagem de erro!" << std::endl;
    return 0;
}
```

Outra opção é utilizar a função `printf` da biblioteca `cstdio`. Esta função é muito semelhante à linguagem C e pode ser útil para quem está migrando de C para C++. Veja um exemplo:

```C++
#include <cstdio>

int main() {
    printf("Imprimindo saída com a função printf\n");
    return 0;
}
```

Por último, podemos também utilizar a função `assert` da biblioteca `cassert` para imprimir saídas de depuração e verificar se uma condição é verdadeira ou falsa. Por exemplo:

```C++
#include <cassert>

int main() {
    int x = 5;
    assert(x > 10);
    return 0;
}
```

Este código irá gerar uma saída de depuração caso a condição `x > 10` seja falsa.

## Veja também

- [Documentação do C++](https://www.cplusplus.com/)
- [Curso básico de C++](https://www.youtube.com/watch?v=8g6YkJqX1Iw)
- [10 maneiras de melhorar sua programação em C++](https://www.freecodecamp.org/news/ten-ways-to-improve-your-programming-in-c/)