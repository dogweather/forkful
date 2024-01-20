---
title:                "Usando expressões regulares"
html_title:           "Gleam: Usando expressões regulares"
simple_title:         "Usando expressões regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Expressões regulares (regex) são uma ferramenta poderosa para encontrar e manipular textos. Programadores as usam para economizar tempo e esforço, permitindo operações complexas de correspondência e substituição de string com poucas linhas de código.

## Como Fazer:

Vamos usar a biblioteca 'regex' do C++ (a partir do C++11). Aqui está um exemplo básico de uso:

```C++
#include <regex>
#include <string>
#include <iostream>

int main() {
    std::string s ("Oi, eu sou um exemplo.");
    std::regex e ("\\bexemplo\\b");   // corresponde a "exemplo" exatamente

    // verificaço de correspondência
    if (std::regex_search(s,e)) {
        std::cout << "Expressão encontrada!" <<std::endl;
    } else {
        std::cout << "Expressão não encontrada." <<std::endl;
    }
    return 0;
}
```

A saída será `Expressão encontrada!`, pois a palavra "exemplo" é encontrada na string `s`.

## Mergulho em Detalhes

Expressões regulares têm uma história longa e interessante, desde os trabalhos iniciais de Stephen Kleene nos anos 50 até sua implementação atual em muitas linguagens de programação modernas.

Existem várias bibliotecas alternativas para usar expressões regulares em C++, como Boost.Regex e PCRE++. Cada uma tem suas próprias peculiaridades e diferenças sutis nas sintaxes de expressões regulares e no desempenho.

Sobre a implementação, a biblioteca 'regex' do C++ usa uma máquina de estados determinística internamente. É isso que permite as correspondências rápidas e eficientes, mesmo para expressões complicadas.

## Veja Também

- https://en.cppreference.com/w/cpp/regex
- https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
- https://www.boost.org/doc/libs/1_75_0/libs/regex/doc/html/index.html

Esses recursos fornecem informações adicionais e mais aprofundadas sobre o uso de expressões regulares em C++.