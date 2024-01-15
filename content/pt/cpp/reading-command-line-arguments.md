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

## Por que
Você já se perguntou como programas em C++ conseguem receber informações diretamente da linha de comando? Aprender a ler argumentos da linha de comando pode ser extremamente útil para tornar seus programas mais interativos e flexíveis.

## Como Fazer
A leitura de argumentos da linha de comando em C++ é feita utilizando a função `main` que possui dois parâmetros, `argc` e `argv`. O `argc` representa o número de argumentos passados e o `argv` é um vetor contendo esses argumentos.

```C++
#include <iostream>

// Função main com dois parâmetros
int main(int argc, char** argv) {
    // Imprime o primeiro argumento após o nome do programa
    std::cout << "O primeiro argumento é: " << argv[1] << std::endl;

    // Imprime o segundo argumento
    std::cout << "O segundo argumento é: " << argv[2] << std::endl;

    return 0;
}
```

Supondo que o programa seja nomeado como `meu_programa.exe` e seja executado com os argumentos `arg1` e `arg2`, a saída seria a seguinte:

```
O primeiro argumento é: arg1
O segundo argumento é: arg2
```

## Mergulho Profundo
Nos sistemas operacionais, sempre que um programa é executado, é criado um processo para ele. Esse processo é responsável por armazenar todos os dados necessários para a execução do programa, incluindo os argumentos da linha de comando. Ao utilizar a função `main` com os parâmetros `argc` e `argv`, você está acessando essas informações armazenadas no processo do programa em execução.

## Veja Também
- [Referência da função main em C++](https://en.cppreference.com/w/cpp/language/main_function)
- [Tutorial de linha de comando em C++](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [Leitura de argumentos em C++ utilizando a biblioteca Boost](https://www.boost.org/doc/libs/1_68_0/doc/html/program_options/tutorial.html)