---
title:                "Lendo argumentos da linha de comando"
html_title:           "C++: Lendo argumentos da linha de comando"
simple_title:         "Lendo argumentos da linha de comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Ler argumentos da linha de comando é uma habilidade importante para programadores em C++. É a capacidade de ler e processar informações fornecidas na linha de comando quando um programa é executado. Isso facilita a interação com o usuário e permite que o programa receba informações essenciais para seu funcionamento.

## How to:

Ao ler os argumentos da linha de comando, é necessário declarar e inicializar a variável argc, que representa o número total de argumentos fornecidos na linha de comando. Também é necessário declarar a variável argv, que é um array de strings que armazena os argumentos individuais.

```C++
int main(int argc, char *argv[]) {
    // código do programa
}
```

Abaixo está um exemplo de um programa simples que lê um argumento da linha de comando e imprime seu valor:

```C++
#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
    if (argc > 1) {
        cout << "O argumento fornecido é: " << argv[1] << endl;
    } else {
        cout << "Nenhum argumento fornecido." << endl;
    }
    return 0;
}
```

Caso o programa seja executado com o argumento "hello", o output será o seguinte:

```
O argumento fornecido é: hello
```

## Deep Dive

A habilidade de ler argumentos da linha de comando tem sido usada desde os primeiros dias da linguagem C++. Além disso, existem alternativas para ler informações durante a execução de um programa, como a entrada padrão e argumentos de função.

A implementação da função main com os argumentos argc e argv é um requisito obrigatório para todos os programas C++, conforme definido pelo padrão da linguagem.

## See Also

- [C++ Reference - Arguments](https://www.cplusplus.com/articles/DEN36Up4/)
- [C++ Tutorial - Command Line Arguments](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [cppreference - main function](https://en.cppreference.com/w/cpp/language/main_function)