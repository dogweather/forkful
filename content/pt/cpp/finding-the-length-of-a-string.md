---
title:                "Encontrando o comprimento de uma string."
html_title:           "C++: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e por quê?
Encontrar o comprimento de uma string é uma tarefa comum em programação, que consiste em determinar o número de caracteres presentes em uma determinada string. Isso é importante para que os programadores possam manipular as strings corretamente e realizar operações específicas em cada caractere.

## Como fazer:
Para encontrar o comprimento de uma string em C++, você pode utilizar a função ```strlen()```, que está presente na biblioteca padrão ```<cstring>```. Veja um exemplo de código abaixo:

```C++
#include <iostream>
#include <cstring>

int main()
{
    char string[] = "Olá, mundo!";
    
    int tamanho = strlen(string); // Armazena o comprimento da string na variável "tamanho"
    
    std::cout << "O comprimento da string é " << tamanho << "." << std::endl;
    
    return 0;
}
```

**Saída:**
```
O comprimento da string é 12.
```

## Mergulho profundo:
A função ```strlen()``` é parte da biblioteca de strings em C++, que foi criada em 1983 para suprir uma lacuna na linguagem. Antes dela, os programadores precisavam escrever suas próprias funções para trabalhar com strings. Além disso, existem outras formas de encontrar o comprimento de uma string, como percorrer o array de caracteres até encontrar o caractere nulo ('\0').

## Veja também:
- Informações adicionais sobre a biblioteca de strings em C++: https://www.cplusplus.com/reference/cstring/
- Comparação entre diferentes linguagens para encontrar o comprimento de strings: https://www.geeksforgeeks.org/comparison-cpp-python-java/