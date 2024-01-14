---
title:    "C++: Imprimindo saída de depuração"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante?

Quando estamos programando em C++, é inevitável que em algum momento nosso código apresente erros ou comportamentos inesperados. Nesses casos, a impressão de saída de depuração pode ser uma ferramenta extremamente útil para identificar e corrigir esses problemas. Ao imprimir informações relevantes durante a execução do programa, podemos analisar e entender melhor o que está acontecendo e, consequentemente, encontrar soluções mais eficientes.

## Como imprimir saída de depuração em C++

Para imprimir saída de depuração em C++, utilizamos a função ```std::cout```, que é parte da biblioteca padrão de input/output da linguagem. Esta função permite imprimir qualquer tipo de dado na saída padrão, como strings, números e até mesmo variáveis de objetos. Veja abaixo um exemplo simples de como utilizar a função ```std::cout```:

```C++
#include <iostream>

// Declaração da função main
int main() {
  // Declaração de uma variável inteira
  int numero = 5;

  // Imprime o valor da variável na saída padrão
  std::cout << "O número é: " << numero << std::endl;

  return 0;
}
```

Este código irá imprimir a mensagem "O número é: 5" na saída padrão. Note que utilizamos o operador ```<<``` para concatenar os valores que serão impressos e, ao final, adicionamos ```std::endl```, que é responsável por adicionar uma quebra de linha. Além disso, é possível utilizar diversos modificadores para formatar a saída de acordo com as nossas necessidades.

## Aprofundando-se na impressão de saída de depuração em C++

Além da simples impressão de valores, a função ```std::cout``` também pode ser utilizada para exibir valores de variáveis durante a execução do programa. O uso do operador ```<<``` nos permite imprimir diversas informações sobre as variáveis, como o endereço de memória, o tipo de dado ou até mesmo seu conteúdo.

Outra dica importante na impressão de saída de depuração é utilizar o comando ```assert()```. Esta função, que também é parte da biblioteca padrão de C++, permite adicionar verificações em determinados pontos do código e, caso a condição não seja cumprida, imprime uma mensagem de erro na saída padrão, facilitando a identificação de problemas durante a execução do programa.

## Veja Também

- [Tutorial de C++ do W3Schools](https://www.w3schools.com/cpp/)
- [Documentação oficial da função std::cout](https://en.cppreference.com/w/cpp/io/cout)
- [Guia de Depuração em C++](https://docs.microsoft.com/pt-br/cpp/cpp/debugging-cpp-in-visual-studio?view=vs-2019)