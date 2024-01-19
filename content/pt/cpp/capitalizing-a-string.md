---
title:                "Capitalizando uma string"
html_title:           "C++: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizar uma String em C++: Um guia passo a passo

## O Que é e Por Quê?

Capitalizar uma string é transformar as primeiras letras das palavras de uma frase para maiúsculas. Programadores fazem isso para melhorar a legibilidade e uniformidade dos dados de texto.

## Como Faz:

Aqui está um exemplo de código simples em C++ para capitalizar uma string:

```C++
#include <locale>
#include <iostream>

// Função para capitalizar uma string
std::string CapitalizeString(const std::string &s) {
    bool newWord = true;
    std::string result;

    for (auto &&c : s) {
        if (std::isspace(c)) {
            newWord = true;
        }
        else if (newWord) {
            c = std::toupper(c);
            newWord = false;
        }
        result += c;
    }
    return result;
}

int main() {
    std::string s = "o rato roeu a roupa do rei";
    std::cout << CapitalizeString(s) << std::endl;
    return 0;
}
```

O resultado da função main acima será:

```C++
O Rato Roeu A Roupa Do Rei
```

## Análise Profunda

Historicamente, capitalizar strings tem sido um conceito importante desde o advento dos sistemas de computador. Isso é essencial no aprimoramento da limpeza e apresentação dos dados.

Existem muitas alternativas para capitalizar uma string em C++, usando bibliotecas como Boost, funções built-in em IDEs ou bibliotecas personalizadas. No exemplo anterior, optamos por utilizar a biblioteca `<locale>` da linguagem padrão.

A implementação é relativamente simples, iterando cada caractere da string, se o caractere é o início de uma palavra (ou seja, precedido por um espaço em branco ou no início da string), ele é transformado em maiúsculo com a função `std::toupper()`.

## Veja Também

Aqui estão alguns recursos adicionais sobre esta temática em C++:

1. Documentation do C++: [Referência std::toupper()](https://en.cppreference.com/w/cpp/string/byte/toupper)
2. StackOverflow: [Convertendo uma string inteira em maiúscula ou minúscula em C++](https://stackoverflow.com/questions/735204/convert-a-string-in-c-to-upper-case)

---
Note que essa abordagem é sensível a idiomas que usam regras especiais para capitalização. Para esses casos, o uso de bibliotecas externas adequadas pode ser necessário.