---
date: 2024-01-20 17:34:06.730575-07:00
description: "Como Fazer: Historicamente, em C++, os arrays de caracteres eram usados\
  \ para strings, e a concatena\xE7\xE3o era manual e propensa a erros. Com a introdu\xE7\
  \xE3o da\u2026"
lastmod: '2024-04-05T21:53:47.222400-06:00'
model: gpt-4-1106-preview
summary: "Historicamente, em C++, os arrays de caracteres eram usados para strings,\
  \ e a concatena\xE7\xE3o era manual e propensa a erros."
title: Concatenando strings
weight: 3
---

## Como Fazer:
```C++
#include <iostream>
#include <string>

int main() {
    std::string nome = "Mundo";
    std::string saudacao = "Olá, " + nome + "!";

    std::cout << saudacao << std::endl; // Saída: Olá, Mundo!

    // Utilizando o método append
    std::string ponto = ".";
    saudacao.append(ponto);
    std::cout << saudacao << std::endl; // Saída: Olá, Mundo!.

    return 0;
}
```

## Mergulho Profundo:
Historicamente, em C++, os arrays de caracteres eram usados para strings, e a concatenação era manual e propensa a erros. Com a introdução da classe `std::string`, tudo ficou mais simples. Alternativas incluem o operador `+`, o método `append()` da classe `std::string` ou até stringstream para casos complexos. Detalhes de implementação variam com a abordagem: operador `+` é intuitivo, `append()` pode ser mais eficiente em certos contextos, e stringstream oferece mais controle e funcionalidade, mas é mais verboso.

## Veja Também:
- Documentação oficial da classe std::string: https://en.cppreference.com/w/cpp/string/basic_string
- Tutorial sobre stringstream: https://www.cplusplus.com/reference/sstream/stringstream/
