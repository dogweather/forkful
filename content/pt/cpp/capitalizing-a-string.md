---
title:                "Capitalizando uma string"
date:                  2024-01-19
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string é o processo de alterar todas as suas letras para maiúsculas. Programadores fazem isso para padronizar dados, melhorar legibilidade ou atender a exigências específicas de um sistema.

## How to:
Veja como capitalizar uma string em C++ com o auxílio da biblioteca padrão (`<algorithm>` e `<cctype>`):

```C++
#include <iostream>
#include <string>
#include <algorithm>
#include <cctype>

int main() {
    std::string texto = "olá, mundo!";
    std::transform(texto.begin(), texto.end(), texto.begin(), 
                   [](unsigned char c){ return std::toupper(c); });

    std::cout << texto << std::endl; // OLÁ, MUNDO!
    return 0;
}
```
Esse código transformará todas as letras de `texto` para maiúsculas e as exibirá.

## Deep Dive
Antes de C++ ter bibliotecas padronizadas, capitalizar strings poderia envolver funções personalizadas que trabalhavam com arrays de caracteres. Com a chegada da STL (Standard Template Library), essa tarefa ficou mais simples e portátil.

Há alternativas para capitalizar uma string em C++. Uma delas é percorrer a string e usar a função `std::toupper` diretamente em cada caractere. Isso pode ser menos eficiente e mais propenso a erros, especialmente com caracteres UTF-8.

Implementar uma função robusta de capitalização requer conhecimento sobre o conjunto de caracteres utilizado. Caracteres fora do intervalo ASCII básico, como letras acentuadas, exigem tratamento diferenciado, algo que `std::toupper` pode não lidar corretamente se a localidade não estiver devidamente configurada.

## See Also
- Documentação do C++: https://en.cppreference.com/
- Tutorial sobre a STL (em inglês): https://www.cplusplus.com/reference/algorithm/transform/
- Discussões sobre o manuseio de UTF-8 em C++: https://utf8everywhere.org/
