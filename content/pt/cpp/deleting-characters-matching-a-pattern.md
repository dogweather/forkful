---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

O processo de excluir caracteres correspondentes a um padrão é uma operação frequente na manipulação de strings em programação. Programadores fazem isso para fazer limpeza de dados, otimizar strings, e implementar funcionalidades de busca.

## Como fazer:

Aqui está um exemplo de como remover todos os caracteres 'a' de uma string usando a biblioteca padrão C++.

```C++
#include <string>
#include <algorithm>

int main() {
    std::string str = "banana";
    str.erase(std::remove(str.begin(), str.end(), 'a'), str.end());
    std::cout << str;

    return 0;
}
```

Saída:

```C++
bnn
```

## Mergulho Profundo

O método mostrado acima utiliza a biblioteca STL (Standard Template Library) que vem sendo parte do C++ desde a sua implementação inicial, e é altamente otimizado para eficiência. Além do STL, há várias bibliotecas de terceiros que podem ser usadas para manipular strings, cada qual com suas próprias vantagens e desvantagens.

A maneira como o código acima funciona é, primeiro, usando a função `std::remove`. Essa função apenas move os elementos que não são iguais a 'a' para a frente na string e retorna um iterador para a nova 'fim' da string. Em seguida, a função `std::string::erase` é usada para remover fisicamente o restante da string.

## Veja Também

1. Documentação da função erase: http://www.cplusplus.com/reference/string/string/erase/
2. Documentação da função remove: http://www.cplusplus.com/reference/algorithm/remove/
3. Informação adicional sobre manipulação de strings: https://pt.wikibooks.org/wiki/Programar_em_C%2B%2B/As_strings