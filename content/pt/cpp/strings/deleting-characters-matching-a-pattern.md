---
date: 2024-01-20 17:41:59.623503-07:00
description: "Como Fazer: Antigamente, a manipula\xE7\xE3o de strings em C++ era feita\
  \ principalmente utilizando fun\xE7\xF5es da biblioteca `<cstring>`, como `strtok()`\
  \ e\u2026"
lastmod: '2024-04-05T21:53:47.214123-06:00'
model: gpt-4-1106-preview
summary: "Antigamente, a manipula\xE7\xE3o de strings em C++ era feita principalmente\
  \ utilizando fun\xE7\xF5es da biblioteca `<cstring>`, como `strtok()` e `strcspn()`,\
  \ com opera\xE7\xF5es manuais de itera\xE7\xE3o e substitui\xE7\xE3o de caracteres."
title: "Excluindo caracteres que correspondem a um padr\xE3o"
weight: 5
---

## Como Fazer:
```c++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string texto = "Exemplo123 com números e !@# símbolos.";
    std::regex padrao("[0-9!@#]"); // Define o padrão para dígitos e símbolos específicos.

    // Remove caracteres que correspondem ao padrão.
    std::string resultado = std::regex_replace(texto, padrao, "");

    std::cout << resultado << std::endl; // Saída: Exemplo com números e  símbolos.

    return 0;
}
```

## Mergulho Profundo
Antigamente, a manipulação de strings em C++ era feita principalmente utilizando funções da biblioteca `<cstring>`, como `strtok()` e `strcspn()`, com operações manuais de iteração e substituição de caracteres. Com a evolução da linguagem, a biblioteca padrão de C++ introduziu a classe `std::string` e, posteriormente, as expressões regulares chegaram com o C++11 na biblioteca `<regex>`. 

Usar `std::regex` é uma forma boa e moderna de se trabalhar com padrões de texto, mas não é a única. Se performance é primordial, é possível que métodos manuais, bibliotecas externas otimizadas como RE2 ou até funcionalidades do C++20, como `std::ranges`, sejam mais adequadas. A compreensão da implementação de expressões regulares também é útil, pois elas podem ter custos computacionais significativos, especialmente em padrões complexos ou strings muito grandes.

## Veja Também
- Documentação oficial do C++ sobre `std::regex`: https://en.cppreference.com/w/cpp/regex
- Tutorial C++ sobre expressões regulares: https://www.learncpp.com/cpp-tutorial/regular-expressions/
- Uma discussão sobre performance de expressões regulares em C++: https://stackoverflow.com/questions/896654/regex-performance
