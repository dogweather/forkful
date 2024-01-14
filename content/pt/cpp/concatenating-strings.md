---
title:    "C++: Concatenando strings"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Por que?

Concatenar strings é uma técnica importante em programação, pois permite a junção de múltiplas strings em uma única string. Isso é especialmente útil em casos onde é necessário formatar texto ou construir mensagens dinamicamente.

## Como Fazer

Para concatenar strings em C++, basta utilizar o operador de soma (+) entre as strings que deseja unir. Veja um exemplo abaixo:

```C++
#include <iostream>

using namespace std;

int main() {
    string nome = "João";
    string sobrenome = "Silva";
    string nome_completo = nome + " " + sobrenome;
    cout << "Seu nome é: " << nome_completo;
    return 0;
}
```

Neste exemplo, as variáveis "nome" e "sobrenome" são concatenadas com o operador de soma e o resultado é armazenado na variável "nome_completo". Ao imprimir essa variável, obtemos a mensagem "Seu nome é: João Silva".

Podemos também concatenar mais de duas strings ou incluir strings fixas na concatenação. Veja abaixo:

```C++
string frase = "Eu gosto de " + "programar em " + "C++.";
```

Neste caso, a variável "frase" terá o valor "Eu gosto de programar em C++.".

## Deep Dive

Em C++, a concatenação de strings é feita através do operador de soma (+), pois o compilador entende que, ao utilizar esse operador com strings, o objetivo é unir as mesmas.

Para evitar erros de compilação, é importante garantir que as strings que serão concatenadas possuem o mesmo tipo. Por exemplo, não é possível concatenar uma string com uma variável do tipo inteiro.

Além disso, é possível utilizar a função `concat` da biblioteca `string` para realizar a concatenação de forma mais explícita. Essa função recebe duas strings como parâmetro e retorna uma nova string com a união das duas.

## Veja Também

- [Tutorial de Strings em C++](https://www.cplusplus.com/doc/tutorial/strings/)
- [Documentação oficial da função concat](http://www.cplusplus.com/reference/string/string/append/)