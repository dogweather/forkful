---
title:                "C++: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação e pode ser útil para várias aplicações. Entender como fazer isso pode facilitar muito o trabalho com strings em seus programas em C++.

## Como fazer

Em C++, a maneira mais comum de encontrar o comprimento de uma string é usando a função `strlen()` da biblioteca de strings padrão. Esta função recebe como argumento a string desejada e retorna o seu comprimento.

```C++
#include <iostream>
#include <cstring> // Incluir biblioteca para usar strlen()

int main() {
    // Definir uma string
    char nome[] = "João";

    // Calcular seu comprimento
    int comprimento = strlen(nome);

    // Imprimir o resultado
    std::cout << "O comprimento da string é: " << comprimento;

    return 0;
}
```

**Saída:**

```
O comprimento da string é: 4
```

Esta função também pode ser usada com strings do tipo `string` da biblioteca padrão `string`, como mostrado no exemplo abaixo:

```C++
#include <iostream>
#include <string> // Incluir biblioteca para usar a classe string

int main() {
    // Definir uma string
    std::string sobrenome = "Silva";

    // Calcular seu comprimento
    int comprimento = sobrenome.length();

    // Imprimir o resultado
    std::cout << "O comprimento da string é: " << comprimento;

    return 0;
}
```

**Saída:**

```
O comprimento da string é: 5
```

## Deep Dive

Para entender melhor como a função `strlen()` funciona, é importante saber que ela conta o número de caracteres da string até encontrar o caractere nulo (`'\0'`), que indica o fim da string. Isso significa que a função não leva em conta o caractere nulo em seu cálculo, por isso é importante garantir que ele esteja presente na string.

Outro aspecto importante é que a função `strlen()` não funciona com strings do tipo `wstring` (strings de caracteres largos) ou `u16string` e `u32string` (strings de código Unicode). Para esses casos, é preciso utilizar outras funções, como `wcslen()` e `u16length()` respectivamente.

## Veja também

- [Documentação da função `strlen()` em C++](https://www.cplusplus.com/reference/cstring/strlen/)
- [Documentação da classe `string` em C++](https://www.cplusplus.com/reference/string/string/)
- [Documentação de funções para manipulação de strings em C++](https://www.cplusplus.com/reference/cstring/)