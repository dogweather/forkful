---
title:                "Utilizando expressões regulares"
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Expressões regulares (RegEx) são padrões usados para encontrar correspondências de texto específicas. Programadores usam RegEx porque simplificam e aceleram o processamento de strings, desde validação até manipulação de dados.

## Como Fazer:
```cpp
#include <iostream>
#include <regex>

int main() {
    std::string frase = "Programar em C++ é incrível!";
    std::regex padrao("(\\bC\\+\\+\\b)"); // Procura a palavra exata 'C++'

    // Substitui 'C++' por 'C#'
    std::string substituto = "C#";
    std::string resultado = std::regex_replace(frase, padrao, substituto); 
    
    std::cout << resultado << std::endl; // Saída: Programar em C# é incrível!

    // Verifica se 'C++' está presente
    bool temCPlusPlus = std::regex_search(frase, padrao);
    std::cout << (temCPlusPlus ? "Encontrado" : "Não encontrado") << std::endl; // Saída: Encontrado
    
    return 0;
}
```

## Mergulho Profundo:
Expressões regulares não são específicas do C++; originaram-se na década de 1950. Alternativas a RegEx incluem análise manual de strings e uso de bibliotecas de processamento de texto especializadas. Na implementação, RegEx no C++ pode ser menos eficiente do que bibliotecas especializadas e a compreensão profunda é crucial para evitar erros e ineficiências.

## Veja Também:
- [cppreference.com - Regular Expressions](https://en.cppreference.com/w/cpp/regex)
- [RegexOne - Aprenda RegEx com exercícios interativos](https://regexone.com/)
- [Documentação oficial da biblioteca <regex>](http://www.cplusplus.com/reference/regex/)
