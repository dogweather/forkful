---
title:                "Concatenando strings"
html_title:           "C++: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Concatenar strings é um recurso importante em programação, permitindo a união de várias strings em uma só. Isso é especialmente útil para lidar com dados e textos de forma eficiente em programas.

## Como Fazer

```C++
#include <iostream>
using namespace std;

int main() {

    // Criando duas variáveis string
    string nome = "João";
    string sobrenome = "Silva";

    // Concatenando as strings
    string nomeCompleto = nome + " " + sobrenome;

    // Imprimindo o resultado
    cout << "Nome completo: " << nomeCompleto << endl;

    return 0;
}

// Output:
// Nome completo: João Silva
```

Para concatenar strings em C++, utilizamos o operador `+` para unir duas ou mais strings. Podemos também adicionar outros caracteres, como espaço vazio, entre as strings para formatar o resultado conforme desejado. É importante lembrar de utilizar a biblioteca `<iostream>` e o uso de `using namespace std` para simplificar a escrita do código.

## Mergulho Profundo

Além do operador `+`, podemos utilizar o método `append()` para concatenar strings em C++. Esse método recebe uma string como parâmetro e acrescenta ao final da string que o invocou. Além disso, é possível utilizar o método `insert()` para inserir uma string em uma posição específica dentro de outra string.

Outro recurso interessante para concatenar strings é a função `getline()`, que permite a leitura de uma linha inteira de input do usuário, incluindo espaços vazios. Combinando essa função com a concatenação de strings, podemos criar um programa capaz de receber dados do usuário e gerar uma saída personalizada.

## Veja Também

- [Documentação oficial do C++ sobre strings](https://www.cplusplus.com/reference/string/)
- [Guia prático para aprender C++](https://www.geeksforgeeks.org/a-practical-implementation-of-c-programming-language/)
- [Tutorial sobre como usar strings em C++](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)