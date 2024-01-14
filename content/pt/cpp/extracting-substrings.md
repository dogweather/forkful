---
title:    "C++: Extraindo subcadeias"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Por que extrair substrings?

Extrair substrings é uma tarefa comum quando se trabalha com strings em C++. Substrings são partes de uma string maior que podem ser usadas para realizar diferentes operações, como busca, manipulação e análise de dados. Neste artigo, vamos explorar como extrair substrings em C++ e aprender algumas dicas e truques úteis ao longo do caminho.

## Como fazer

Em C++, existem duas maneiras de extrair substrings de uma string maior: usando a função `substr()` ou acessando os elementos individuais da string usando o operador de índice `[]`.

### Usando a função `substr()`

A função `substr()` permite extrair uma parte específica de uma string, com base no índice inicial e no comprimento desejado. Considere o exemplo a seguir:

```C++
#include <iostream>
using namespace std;

int main() {
    string frase = "Este é um exemplo de uma string.";
    string substring = frase.substr(8, 7);

    cout << substring << endl; // saída: "exemplo"
    return 0;
}
```

No código acima, usamos a função `substr()` para extrair uma substring a partir do índice 8 (que corresponde à letra "e" na palavra "exemplo") com um comprimento de 7 caracteres (o que inclui o espaço após a palavra "é"). O resultado é a palavra "exemplo" sendo impressa na tela.

### Acessando elementos individuais

Outra maneira de extrair substrings é acessando os elementos individuais da string com o operador de índice `[]`. Vamos ver um exemplo:

```C++
#include <iostream>
using namespace std;

int main() {
    string texto = "Frase de exemplo";
    string substring = "";

    for(int i = 6; i < 14; i++) {
        substring += texto[i];
    }

    cout << substring << endl; // saída: "de exemplo"
    return 0;
}
```

Neste exemplo, usamos um loop `for` para percorrer a string `texto` e armazenar os caracteres da posição 6 até a posição 13 na substring. O resultado é a substring "de exemplo" sendo impressa na tela.

É importante lembrar que o índice de um array em C++ começa em 0, então a primeira letra de uma string tem o índice 0, a segunda tem o índice 1 e assim por diante.

## Aprofundando-se

Agora que aprendemos duas maneiras de extrair substrings em C++, vamos dar uma olhada em alguns detalhes importantes para lembrar quando se trabalha com substrings.

- O primeiro argumento da função `substr()` é o índice inicial, mas também pode ser uma contagem negativa para extrair uma substring do final da string.
- A função `substr()` também pode ser usada para extrair o restante de uma string a partir de um determinado índice, se o segundo argumento for omitido.
- Ao usar o operador de índice `[]`, é possível passar expressões matemáticas para calcular o índice de uma substring. Por exemplo, podemos usar `texto[texto.length() / 2]` para obter o caractere no meio da string `texto`.
- O C++ também possui outras funções úteis para manipulação de strings, como `find()`, `replace()` e `erase()` que podem ser usadas em conjunto com a extração de substrings para realizar operações mais complexas.

## Veja também

- [Documentação da função `substr()` em C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Outros métodos de manipulação de strings em C++](https://www.programiz.com/cpp-programming/library-function/cstring/)
- [Um guia completo sobre strings em C++](https://www.codecademy.com/learn/learn-c-plus-plus/modules/learn-cpp-strings)