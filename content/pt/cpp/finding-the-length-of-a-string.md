---
title:                "Encontrando o comprimento de uma string"
html_title:           "C++: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string pode ser útil em casos onde você precisa contar o número de caracteres ou verificar se uma string possui o comprimento mínimo ou máximo permitido. É uma tarefa comum em programação e pode ajudar a garantir a validade dos dados inseridos pelo usuário.

## Como fazer

Para encontrar o comprimento de uma string em C++, podemos usar a função `length()` ou `size()` da classe `string`. Veja um exemplo abaixo:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string myString = "Hello world!";
    int length = myString.length();
    cout << "O comprimento da string é: " << length << endl;
    return 0;
}
```

Saída:
```
O comprimento da string é: 12
```

Neste exemplo, declaramos uma variável `myString` do tipo `string`. Em seguida, usamos a função `length()` para encontrar o comprimento da string e atribuir o valor à variável `length`. Por fim, imprimimos o resultado na tela utilizando `cout`.

Além da função `length()`, também podemos usar `size()` que tem o mesmo efeito. As duas funções são praticamente idênticas e podem ser usadas de forma intercambiável.

## Aprofundando

Para entender melhor como a função `length()` funciona em C++, é importante saber que a classe `string` possui uma propriedade chamada `size` que armazena o tamanho da string. A função `length()` é apenas uma forma de acessar essa propriedade.

Embora as funções `length()` e `size()` sejam comumente usadas para encontrar o comprimento de strings, também é possível usá-las para outros tipos de dados, como arrays e vectors. Basta garantir que o tipo de variável em que elas serão usadas possua uma propriedade `size`.

Outro ponto importante é que, apesar de a maioria das strings possuírem a mesma função `length()`, algumas linguagens de programação podem ter funções diferentes para encontrar o comprimento de strings. Por isso, é sempre bom consultar a documentação da linguagem que você está utilizando para garantir que está usando a função correta.

## Veja também

- [Documentação oficial do C++ sobre std::string](https://en.cppreference.com/w/cpp/string/basic_string)
- [Tutorial de C++ básico sobre strings](https://www.geeksforgeeks.org/strings-in-c-2/)
- [Como contar o número de caracteres em  C++](https://www.programiz.com/cpp-programming/examples/count-character)