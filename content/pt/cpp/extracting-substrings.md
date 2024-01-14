---
title:                "C++: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em C++?

Ao trabalhar com strings em C++, muitas vezes precisamos extrair partes específicas dessas strings. Isso pode ser necessário para realizar operações específicas, como busca, comparação ou formatação de dados. Felizmente, a linguagem C++ possui um conjunto de ferramentas robustas para extrair substrings de maneira eficiente e flexível.

## Como extrair substrings em C++

Extrair substrings em C++ pode ser feito de diversas maneiras, dependendo da sua necessidade. A seguir, demonstraremos dois métodos comuns para realizar essa tarefa: utilizando a função `substr()` e utilizando índices de acesso direto.

### Usando a função `substr()`

Para extrair uma substring utilizando a função `substr()`, primeiro precisamos ter uma string original. Em seguida, utilizando a sintaxe `string.substr(posição, tamanho)`, podemos especificar a posição inicial e o tamanho da substring que queremos extrair. Veja o exemplo abaixo:

```
#include <iostream>
using namespace std;

int main() {
  string frase = "Isso é uma string";
  string substring = frase.substr(5, 4);
  cout << substring << endl;
  // Saída: é uma
}

```

Nesse caso, a posição inicial especificada foi `5` e o tamanho escolhido foi de `4` caracteres. Essa função retornará a substring a partir da posição `5` da string original, com `4` caracteres de tamanho.

### Usando índices de acesso direto

Outra maneira de extrair substrings em C++ é acessando diretamente os índices da string original. Cada caractere de uma string possui um índice correspondente, onde o primeiro caractere possui o índice `0`, o segundo o índice `1` e assim por diante. Para extrair uma substring a partir desses índices, basta determinar a posição inicial e final desejada e percorrer a string original a partir desses índices.

```
#include <iostream>
using namespace std;

int main() {
  string frase = "Isso é uma string";
  string substring = "";
  for (int i = 5; i < 9; i++) {
    substring += frase[i];
  }
  cout << substring << endl;
  // Saída: é uma
}

```

Nesse exemplo, utilizamos um loop for para percorrer os índices da substring desejada, adicionando cada caractere encontrado à nossa substring final.

## Aprofundando-se na extração de substrings

Além dos métodos mencionados acima, é importante mencionar que a linguagem C++ possui uma variedade de funções e algoritmos dedicados à manipulação de strings. É possível, por exemplo, utilizar expressões regulares para extrair substrings de acordo com um padrão pré-determinado, ou ainda utilizar células individuais de strings para criar novas substrings.

## Veja também

- [Documentação do substr na string C++](https://www.cplusplus.com/reference/string/string/substr/)
- [Guia prático sobre manipulação de strings em C++](https://www.learncpp.com/cpp-tutorial/63-character-strings/)