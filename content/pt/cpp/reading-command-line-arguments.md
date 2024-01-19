---
title:                "Lendo argumentos de linha de comando"
html_title:           "Arduino: Lendo argumentos de linha de comando"
simple_title:         "Lendo argumentos de linha de comando"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Comandos de linha (Command line arguments em inglês) são especificações que um programa recebe quando ele é iniciado, ou seja, informações adicionais que orientam o comportamento do programa. Programadores usam isso para adicionar flexibilidade aos programas, permitindo aos usuários especificar comportamentos personalizados no momento da execução.

## Como Fazer:
```C++
#include <iostream>
using namespace std;

int main(int argc, char** argv) {
    cout << "Número de argumentos: " << argc << endl;
    for(int i = 0; i < argc; ++i) {
        cout << "Argumento " << i << ": " << argv[i] << endl;
    }
    return 0;
}
```
A execução desse código com argumentos "arg1 arg2 arg3" na linha de comando produziria:
```
Número de argumentos: 4
Argumento 0: ./programa
Argumento 1: arg1
Argumento 2: arg2
Argumento 3: arg3
```

## Mais Detalhes:
Os argumentos da linha de comando são uma prática antiga, vinda dos primórdios do UNIX. Alternativas atuais para passar informações para programas incluem o uso de um arquivo de configurações ou entrada interativa do usuário durante a execução.

Em C++, os argumentos da linha de comando são passados para a função 'main' como um array de strings. O primeiro argumento, argv[0], normalmente é o nome do programa. 'argc' é o número de argumentos passados, incluindo o próprio nome do programa.

## Veja Também:
- Tutorial do cplusplus.com sobre argumentos da linha de comando: http://www.cplusplus.com/articles/DEN36Up4/
- StackOverflow discute quando usar argumentos da linha de comando: https://stackoverflow.com/questions/3024317/when-should-i-use-the-command-line-arguments