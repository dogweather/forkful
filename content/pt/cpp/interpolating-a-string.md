---
title:                "Interpolando uma string"
date:                  2024-01-20T17:50:29.997778-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolando uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Interpolar uma string significa incorporar variáveis ou expressões em meio a um texto fixo para criar mensagens dinâmicas. Programadores fazem isso para personalizar saídas, automatizar mensagens, e tornar o código mais legível e manutenível.

## Como Fazer:
```C++
#include <iostream>
#include <string>

int main() {
    std::string nome = "João";
    int idade = 30;

    // Usando o operador '+' para concatenar strings e variáveis
    std::cout << "Olá, " + nome + "! Você tem " + std::to_string(idade) + " anos." << std::endl;

    // Usando C++20 std::format (se disponível)
    // std::cout << std::format("Olá, {}! Você tem {} anos.", nome, idade) << std::endl;

    return 0;
}
```

Saída esperada:
```
Olá, João! Você tem 30 anos.
```

## Mergulho Profundo
Antes do C++20, a interpolação de strings em C++ envolvia a concatenação manual usando o operador `+` ou a função `append`, ou usando streams de entrada/saída. Hoje, podemos usar `std::format`, uma funcionalidade similar ao `printf` do C, mas mais seguro e fácil de usar. Alternativas incluem bibliotecas de terceiros como `fmt` ou `boost.format`.

Detalhes de implementação:
- Usar '+' é simples, mas pode ser ineficiente, especialmente para muitas variáveis ou loops grandes.
- `std::format` é mais elegante e eficiente, mas requer o C++20.
- Bibliotecas de terceiros podem oferecer funcionalidades avançadas, mas aumentam a dependência do projeto.

## Veja Também
- Documentação oficial de `std::format`: https://en.cppreference.com/w/cpp/utility/format/format
- Biblioteca `fmt`: https://github.com/fmtlib/fmt
- Tutorial sobre streams em C++: https://www.cplusplus.com/doc/tutorial/basic_io/