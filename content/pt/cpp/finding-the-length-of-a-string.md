---
title:                "C++: Encontrando o comprimento de uma sequência"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é uma habilidade fundamental em programação. Saber como fazer isso pode ajudá-lo a manipular e analisar dados de forma mais eficiente, além de tornar seu código mais dinâmico e eficaz.

## Como fazer

Existem várias maneiras de encontrar o comprimento de uma string em C++, mas a mais comum é usando a função `size()` da biblioteca `string`. Veja um exemplo de código abaixo:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string nome = "Maria";
  cout << "O comprimento da string \"" << nome << "\" é: " << nome.size() << endl;

  return 0;
}
```

Output:
```
O comprimento da string "Maria" é: 5
```

Neste exemplo, usamos a função `size()` para encontrar o tamanho da string armazenada na variável `nome`. É importante lembrar que em C++, uma string é considerada como um objeto, portanto, precisamos utilizar `nome.size()` ao invés de simplesmente `size(nome)`.

Outra forma de encontrar o comprimento de uma string é utilizando um loop `for`. Isso nos permite percorrer cada caractere da string e contar quantos são até chegar ao final. Veja um exemplo:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
  string nome = "João";
  int tamanho = 0;

  for (int i = 0; nome[i] != '\0'; i++) {
    tamanho++;
  }

  cout << "O comprimento da string \"" << nome << "\" é: " << tamanho << endl;

  return 0;
}
```

Output:
```
O comprimento da string "João" é: 4
```

Neste exemplo, usamos um loop `for` para percorrer a string e incrementar a variável `tamanho` a cada iteração até que seja encontrado o caractere nulo (`\0`) que indica o final da string.

## Deep Dive

Se você está se perguntando por que a função `size()` retorna um valor do tipo `size_t`, isso acontece porque essa função é implementada como um inteiro sem sinal (não pode ter valores negativos) e é capaz de armazenar valores maiores do que um inteiro comum.

Outra coisa importante a saber é que a função `size()` só retorna o tamanho da string em bytes, ou seja, ela pode ser diferente do número de caracteres dependendo do tipo de caractere usado (ex: letras Unicode ocupam mais de um byte). Para encontrar o número exato de caracteres em uma string, é necessário usar a função `length()`.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre strings em C++:

- [Documentação oficial da função size()](https://www.cplusplus.com/reference/string/string/size/)
- [Tutorial de strings em C++](https://www.geeksforgeeks.org/string-class-in-c/)
- [Funcionamento interno da função size()](https://medium.com/@SasiPrabhu/c-std-string-whats-the-difference-between-size-and-length-ff0c2d2f1941)