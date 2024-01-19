---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O que & por quê?
Procurar e substituir texto é uma ação freqüente em programação usada para encontrar padrões específicos dentro de strings e substituí-los por um novo texto. Os programadores fazem isso para refatorar código, manipular dados e melhorar a eficiência.

## Como fazer:
Usaremos a função `std::replace` do C++ para substituir caracteres em uma string:

```C++
#include <algorithm>
#include <string>
#include <iostream>

int main() {
    std::string s = "Olá mundo!";
    std::replace(s.begin(), s.end(), 'o', 'a');

    std::cout << s << std::endl;
    return 0;
}
```
A saída deverá ser: 
```
"Ola mund!"
```

## Mergulhando fundo:
Historicamente, a busca e substituição de texto existe desde os primeiros editores de texto, fornecendo aos programadores a capacidade de fazer alterações em larga escala no código. No C++ moderno, podemos usar várias abordagens além do `std::replace`, como `std::regex_replace` para substituições baseadas em regex ou funções de biblioteca de terceiros que podem oferecer mais características.

A implementação do `std::replace` faz um loop através do intervalo fornecido (aqui, do início ao fim da string) e substitui cada ocorrência do segundo argumento pelo terceiro. A eficiência dessa operação é O(n), onde n é o comprimento da string.

## Veja também:
Para mais informações sobre funções de manipulação de texto em C++:
- std::replace [https://www.cplusplus.com/reference/algorithm/replace/](https://www.cplusplus.com/reference/algorithm/replace/)
- std::regex_replace [https://www.cplusplus.com/reference/regex/regex_replace/](https://www.cplusplus.com/reference/regex/regex_replace/) 
- Guia completo do C++ Moderno [https://www.learncpp.com/](https://www.learncpp.com/)