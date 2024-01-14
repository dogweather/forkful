---
title:    "C++: Lendo argumentos da linha de comando"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Por que ler argumentos de linha de comando em C ++?

Ao programar em C ++, é importante saber como lidar com argumentos de linha de comando. Esses argumentos são informações inseridas pelo usuário no momento em que o programa é executado. Saber como ler e utilizar esses argumentos pode tornar seu código mais flexível e permitir que o usuário controle melhor a execução do programa.

## Como ler argumentos de linha de comando em C ++

Para ler argumentos de linha de comando, você precisará usar o parâmetro "argc" (contador de argumentos) e "argv" (lista de argumentos) na função "main". O parâmetro "argc" indica o número de argumentos inseridos pelo usuário, enquanto "argv" é um array de strings que contém os argumentos.

```C++
#include <iostream>
using namespace std;

int main(int argc, char* argv[])
{
    // Imprime o número de argumentos
    cout << "Número de argumentos: " << argc << endl;

    // Imprime cada argumento
    for(int i = 0; i < argc; i++)
    {
        cout << "Argumento " << i << ": " << argv[i] << endl;
    }

    return 0;
}
```
Exemplo de entrada: `./programa argumento1 argumento2`
Saída:
```
Número de argumentos: 3
Argumento 0: ./programa
Argumento 1: argumento1
Argumento 2: argumento2
```
Com isso, você pode utilizar os argumentos em seu código da forma que melhor lhe convier.

## Aprofundando-se em leitura de argumentos de linha de comando

Para uma utilização mais avançada, é possível utilizar bibliotecas como "getopt" ou "Boost" para facilitar a leitura e o tratamento dos argumentos. Essas bibliotecas oferecem funções mais específicas, como validação de argumentos, opções de linha de comando e suporte a diferentes tipos de dados.

Além disso, é importante ter em mente que os argumentos são lidos na ordem em que são inseridos pelo usuário. Você pode utilizar a função "strcmp" para comparar argumentos e executar ações diferentes com base neles.

## Veja também

- [Documentação do C++: argc e argv]("https://www.cplusplus.com/articles/DEN36Up4/")
- [Biblioteca "getopt" em C++]("https://www.gnu.org/software/libc/manual/html_node/Argument-Syntax.html")
- [Biblioteca "Boost" em C++]("https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html")

Esperamos que este artigo tenha sido útil para compreender melhor a leitura de argumentos de linha de comando em C ++. Comece a utilizá-los em seus programas e explore as diferentes possibilidades que eles oferecem!