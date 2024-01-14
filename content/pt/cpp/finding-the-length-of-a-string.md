---
title:    "C++: Encontrando o comprimento de uma string"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que
Encontrar o comprimento de uma string é uma tarefa comum em programação e é essencial para trabalhar com dados de texto. Saber como encontrar o tamanho de uma string é um conhecimento básico importante para programadores iniciantes e experientes.

## Como Fazer
Para encontrar o comprimento de uma string em C++, podemos usar a função integrada `strlen ()` ou percorrer a string e contar manualmente o número de caracteres.

### Usando a função `strlen ()`
A função `strlen ()` está disponível na biblioteca `string.h` e retorna o número de caracteres em uma string. Vamos dar uma olhada em um exemplo de código usando essa função:

```C++
#include <iostream>
#include <string.h> // incluindo a biblioteca necessária

using namespace std;

int main() {
    char str[] = "Hello World"; // declarando uma string
    int length = strlen(str); // usando a função strlen para encontrar o comprimento da string
    cout << "O comprimento da string é: " << length << endl;
    return 0;
}

// Output: O comprimento da string é: 11
```

### Contando manualmente os caracteres
Outra maneira de encontrar o comprimento de uma string é percorrer a string e contar manualmente o número de caracteres. Vamos ver um exemplo de código para entender melhor:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string str = "Hello World"; // declarando uma string
    int length = 0;

    // percorrendo a string e contando o número de caracteres
    for (int i = 0; str[i] != '\0'; i++) {
        length++;
    }

    cout << "O comprimento da string é: " << length << endl;
    return 0;
}

// Output: O comprimento da string é: 11
```

## Mergulho Profundo
Em C++, cada caractere em uma string é armazenado em uma posição de memória separada, com um caractere nulo (`\0`) no final para indicar o término da string. A função `strlen ()` conta o número de caracteres até encontrar o `\0`, enquanto o método de contagem manual usa esse caractere para determinar o comprimento da string.

## Veja também
- [Funções de String no C++](https://www.cplusplus.com/reference/cstring/)
- [Tutorial de C++ para Iniciantes](https://www.learn-c.org/) 
- [Documentação do C++](https://isocpp.org/)