---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extraindo Substrings em C++

## O Que & Por quê?

Extrair substrings é o processo de obter uma pequena parte de uma string, a partir de uma posição e comprimento especificados. Nós, programadores, fazemos isso para manipular e processar informações dentro de textos maiores de forma eficiente.

## Como Fazer:

Você pode extrair substrings em C++ usando o método `substr`, da classe `std::string`. 
Vamos a um exemplo:
```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Olá, mundo!";
    std::string sub = str.substr(5, 5);
    std::cout << sub << std::endl; // Saída: "mundo"
    return 0;
}
```

Isso vai extrair a substring começando na posição 5 (os índices em C++ começam do 0) e contar 5 caracteres.

## Mergulho Profundo:

A função `substr` existe em C++ desde a primeira versão da STL (Standard Template Library), que foi incorporada no padrão C++98. É uma das funções básicas de manipulação de strings, mas poderosa e amplamente utilizada.

Alternativamente, você poderia usar funções como `memmove` ou `strncpy` do C, mas são menos intuitivas e mais propensas a erros.

A implementação da `std::string::substr` normalmente cria uma nova string contendo os caracteres desejados. A eficiência disso depende da implementação específica da biblioteca padrão que você está usando, mas geralmente é um processo bastante rápido.

## Veja Também:

Para mais detalhes sobre manipulação de strings em C++, consulte a documentação oficial:

1. [cplusplus.com reference on std::string::substr](http://www.cplusplus.com/reference/string/string/substr/)
2. [cppreference.com on std::string](https://en.cppreference.com/w/cpp/string/basic_string)
3. [Bjarne Stroustrup's website](http://www.stroustrup.com/C++.html)

Lembre-se que a prática leva à perfeição. Então, continue programando e melhorando suas habilidades.