---
title:                "C++: Extração de Substrings"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que usar a extração de substrings em C++?

Existem várias razões pelas quais você pode querer extrair substrings em C++. Você pode estar trabalhando com uma grande quantidade de texto e precisar apenas de uma parte específica, ou pode precisar manipular strings de diferentes maneiras para sua aplicação. Além disso, a extração de substrings é uma habilidade importante ao trabalhar com linguagens de programação como C++.

## Como fazer em C++?

A extração de substrings em C++ pode ser feita facilmente usando a função `substr()` da biblioteca `<string>`. Esta função aceita dois parâmetros: o primeiro é a posição inicial da substring e o segundo é o comprimento da mesma. Abaixo está um exemplo de código mostrando como podemos extrair uma substring de uma string e imprimir o resultado:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {

  string nome = "Carlos";
  string sub_string = nome.substr(1, 3);

  cout << "Substring: " << sub_string << endl;
  // saída: "ubS"

  return 0;
}
```

Como mostrado no exemplo, a função `substr()` retorna a parte da string que começa na posição especificada e tem o comprimento especificado. Você pode usar variáveis e loops para manipular e extrair diferentes substrings de uma string.

## Mais informações sobre extração de substrings

Ao extrair substrings em C++, é importante ter em mente que a posição inicial começa a partir de 0 e o comprimento inclui a posição inicial. Por exemplo, se usarmos `substr(0, 3)`, estaríamos extraindo os primeiros três caracteres da string.

Outra coisa importante a mencionar é que a função `substr()` não altera a string original, mas retorna uma nova. Portanto, se você quiser manter a string original intacta, certifique-se de salvar o resultado da função em uma nova variável.

## Veja também
- [Documentação da função `substr()`](https://www.cplusplus.com/reference/string/string/substr/)
- [Tutorial sobre strings em C++](https://www.geeksforgeeks.org/c-string-classes-and-their-applications/)