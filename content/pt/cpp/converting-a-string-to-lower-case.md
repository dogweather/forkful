---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
A conversão de uma string em letras minúsculas é o processo de mudar todas as letras maiúsculas em uma string para as suas respectivas letras minúsculas. Os programadores fazem isso para normalizar os dados de entrada, facilitando a comparação e busca de strings.

## Como Fazer:
Aqui está um exemplo simples de como você poderia converter uma string em letras minúsculas em C++:

```C++
#include <iostream>
#include <algorithm>
#include <string>
    
int main() {
    std::string data = "Texto Exemplo";
    std::transform(
        data.begin(), 
        data.end(), 
        data.begin(), 
        ::tolower
    );
    std::cout << data;
    return 0;
}
```
Ao executar esse código, a saída será: texto exemplo.

## Mergulho Profundo
A conversão de strings em letras minúsculas tem sido uma prática comum desde os primeiros dias de programação. Isso é útil em muitos cenários de programação, como pesquisas de texto, combinação de strings, classificação e muito mais.

Em termos de alternativas, você também pode usar um loop for para percorrer cada personagem na string e usar a função `tolower()` nela. Mas, usando `std::transform`, nós obtemos uma solução mais elegante e concisa.

Em relação aos detalhes da implementação, a função `std::transform` aplica a função `tolower` a cada caractere na string, imutavelmente. Se o caractere for uma letra maiúscula, `tolower` a transformará em minúscula. Se já for uma minúscula ou um caracter não alfabético, deixará como está.

## Veja Também
Caso você queira se aprofundar, aqui estão alguns links úteis sobre manipulação de string em C++:

- [Site oficial do C++](http://www.cplusplus.com/reference/string/string/)
- [Documentação std::transform no cppreference](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Documentação tolower na página do cplusplus](http://www.cplusplus.com/reference/cctype/tolower/)