---
title:                "C++: Lendo argumentos da linha de comando"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Por que ler argumentos da linha de comando é importante para programadores C++

Quando se trata de programação C++, saber como ler e utilizar argumentos de linha de comando é uma habilidade essencial. Isso permite que os programadores criem programas mais versáteis e interativos, além de facilitar a integração com outros sistemas e ferramentas. Neste post, vamos mergulhar nesse tópico e abordar como ler argumentos de linha de comando no C++.

## Como ler argumentos de linha de comando em C++

Existem várias maneiras de ler argumentos de linha de comando em C++, mas vamos nos concentrar no método mais simples e popular. Primeiramente, é necessário incluir a biblioteca <iostream> para permitir a entrada e saída de dados, e a biblioteca <cstdlib> para acessar a função de conversão de texto para números:

```C++
#include <iostream>
#include <cstdlib>
```

Em seguida, é preciso declarar a função main, que é o ponto de entrada do programa. A função main também recebe dois parâmetros: argc, que é o número de argumentos passados pela linha de comando, e argv, que é um vetor que contém esses argumentos. 

```C++
int main(int argc, char* argv[]){
    //Código do programa vai aqui
}
```

Para acessar os argumentos individualmente, basta utilizar o vetor argv[], passando o índice do argumento desejado. Por exemplo, para imprimir no console o primeiro argumento passado, podemos fazer o seguinte:

```C++
std::cout << argv[0] << std::endl;
```

É importante lembrar que o primeiro elemento do vetor é sempre o nome do programa, seguido dos argumentos passados.

Para converter o argumento de texto para um número inteiro, podemos utilizar a função std::atoi() da biblioteca <cstdlib>. Por exemplo, se o segundo argumento for um número inteiro, podemos armazená-lo em uma variável do tipo int da seguinte forma:

```C++
int num = std::atoi(argv[1]);
```

## Profundidade na leitura de argumentos de linha de comando

Além de acessar argumentos individuais, também é possível percorrer todos os argumentos em um loop. Isso pode ser útil quando o número de argumentos é variável ou quando precisamos realizar a mesma operação em todos eles.

```C++
for(int i = 0; i < argc; i++){
    //Acessar e utilizar os argumentos aqui
}
```

Também é importante lembrar que os argumentos passados pela linha de comando são lidos como texto, então é necessário fazer as devidas conversões para utilizar como números ou outros tipos de dados.

## Veja também

- [Tutorial de C++ da W3Schools](https://www.w3schools.com/cpp)
- [Documentação da função std::atoi()](https://www.cplusplus.com/reference/cstdlib/atoi/)