---
date: 2024-01-20 17:45:15.732749-07:00
description: "Como Fazer: Historicamente, extrair substrings \xE9 fundamental nas\
  \ linguagens de programa\xE7\xE3o por sua utilidade em diferentes dom\xEDnios, desde\
  \ primeiras\u2026"
lastmod: '2024-04-05T21:53:47.219147-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, extrair substrings \xE9 fundamental nas linguagens de programa\xE7\
  \xE3o por sua utilidade em diferentes dom\xEDnios, desde primeiras aplica\xE7\xF5\
  es em scripts at\xE9 sistemas complexos de hoje."
title: Extraindo substrings
weight: 6
---

## Como Fazer:
```C++
#include <iostream>
#include <string>

int main() {
    std::string texto = "C++ é muito legal!";
    std::string substring = texto.substr(4, 2); // Pega "é "
    
    std::string outra_substring = texto.substr(7); // Pega "muito legal!"
    
    std::cout << substring << std::endl; // Saída: é 
    std::cout << outra_substring << std::endl; // Saída: muito legal!
    
    return 0;
}
```

## Mergulho Profundo:
Historicamente, extrair substrings é fundamental nas linguagens de programação por sua utilidade em diferentes domínios, desde primeiras aplicações em scripts até sistemas complexos de hoje. Ao longo do tempo, as bibliotecas padrão, como `<string>` em C++, evoluíram para fornecer suporte robusto e simplificado para tais operações. Outras maneiras de realizar a extração de substrings incluem usar funções como `find()` para localizar posições e operações de iterators. Em termos de implementação, substrings são tipicamente tratadas como ponteiros ou referências para evitar cópias desnecessárias de grandes pedaços de texto, algo importante quando consideramos a eficiência.

## Veja Também:
- Documentação oficial da classe `std::basic_string`: https://en.cppreference.com/w/cpp/string/basic_string
- Tutorial de `std::string`: http://www.cplusplus.com/reference/string/string/
- Artigo sobre eficiência de strings em C++: https://www.bfilipek.com/2018/07/string-view-perf-followup.html
