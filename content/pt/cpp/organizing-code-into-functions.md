---
title:                "Organizando o código em funções"
aliases:
- pt/cpp/organizing-code-into-functions.md
date:                  2024-01-26T01:09:39.810653-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Dividir o código em funções significa esculpir seu código em blocos menores e reutilizáveis. Fazemos isso para evitar repetição, tornar nosso código legível e simplificar a depuração e o teste. Funções bem organizadas podem ser como ter uma caixa de ferramentas ordenadas e devidamente etiquetadas, prontas para usar e compartilhar.

## Como fazer:
Vamos pegar uma tarefa comum: calcular a área de um círculo. Em vez de escrever a mesma fórmula todas as vezes, encapsulamos ela em uma função.

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "Área do círculo com raio " << r << " é " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

Saída de exemplo:
```
Área do círculo com raio 5 é 78.5397
```

## Aprofundamento
Historicamente, procedimentos e funções foram o alicerce da programação estruturada, promovida na década de 1960 para combater os problemas do "código espaguete" em linguagens de programação imperativas anteriores. Alternativas como OOP (Programação Orientada a Objetos) avançam mais ao associar essas funções com estruturas de dados. Em C++, temos funções regulares, métodos de classe (incluindo métodos estáticos), lambdas e funções de templates, cada um oferecendo benefícios diferentes. Implementar funções bem organizadas geralmente envolve aderir a princípios como DRY ("Don't Repeat Yourself") e SRP (Princípio da Responsabilidade Única), o que significa que cada função faz apenas uma coisa e faz bem feito.

## Veja Também
Para mais sobre funções em C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

Para princípios de design relacionados a funções:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

Saiba mais sobre lambdas e uso avançado de funções:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
