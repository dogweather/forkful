---
title:                "C++: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que Deletar Caracteres Correspondentes a um Padrão?

Há várias razões pelas quais alguém pode precisar deletar caracteres correspondentes a um padrão em C++. Alguns dos motivos mais comuns incluem formatação de dados, remoção de caracteres inválidos ou substituição de caracteres específicos. Em alguns casos, isso também pode ser necessário para garantir a segurança do programa ou otimizar o desempenho.

## Como Fazer

Para deletar caracteres correspondentes a um padrão em C++, podemos usar a função `erase()` da biblioteca de strings `std::string`. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <string>

int main() {
    // Definindo uma string com um padrão a ser removido
    std::string str = "Hello, World!#@";

    // Deletando todos os caracteres não-alfanuméricos
    str.erase(std::remove_if(str.begin(), str.end(), [](char c){
        return !std::isalnum(c);
    }), str.end());

    // Imprimindo o resultado
    std::cout << str; // Output: HelloWorld

    return 0;
}
```

Neste exemplo, usamos a função `std::remove_if` para selecionar todos os caracteres que não são alfanuméricos e a função `erase()` para removê-los da string. O resultado é a string "HelloWorld" sem os caracteres especiais.

## Aprofundando

A função `erase()` não é a única maneira de deletar caracteres correspondentes a um padrão em C++. Também podemos usar expressões regulares ou outras funções da biblioteca de strings, como `replace` ou `substr`. Além disso, é importante estar atento a possíveis problemas de desempenho ao usar certas técnicas de remoção de caracteres, especialmente para grandes strings.

## Veja Também

- Documentação da função `erase()`: https://www.cplusplus.com/reference/string/string/erase/
- Exemplos de expressões regulares em C++: https://www.geeksforgeeks.org/regular-expressions-in-c-c/
- Mais informações sobre manipulação de strings em C++: https://www.geeksforgeeks.org/string-manipulation-in-cc/