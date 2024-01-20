---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## O Que é & Por Que Usar?
Interpolação de strings é o processo de substituir marcadores de posição em uma string por seus respectivos valores de dados. Essa prática é comum porque nos ajuda a evitar a concatenação de strings, que pode tornar o código menos legível em comparação à interpolação.

## Como Fazer:
No C++ atual, podemos fazer a interpolação de strings usando a biblioteca `fmt`:

```C++
#include <fmt/core.h>

int main() {
    std::string nome = "Joao";
    int idade = 30;
    std::string frase = fmt::format("Meu nome é {} e eu tenho {} anos.", nome, idade);
    fmt::print("{}", frase);
    return 0;
}
```
A saída será: `Meu nome é Joao e eu tenho 30 anos.` 

## Mergulho Profundo
A interpolação de strings não é uma ideia nova. Já era usada em diversas linguagens de programação antes de ser introduzida no C++. Alternativamente, você pode usar a concatenação de strings ou a biblioteca `sprintf`, mas ambas podem resultar em código mais complexo e menos legível.

No que diz respeito aos detalhes de implementação, a Classe `fmt::format` faz a interpolação de strings internamente, substituindo os marcadores de posição pelas respectivas variáveis na ordem fornecida. A Classe `fmt::format` é capaz de realizar estas operações de forma eficiente e segura.

## Ver Também
1. [Documentação oficial fmtlib](https://fmt.dev/latest/index.html)
3. [Stack Overflow: Interpolação de strings em C++](https://stackoverflow.com/questions/2342162/stdstring-formatting-like-sprintf)