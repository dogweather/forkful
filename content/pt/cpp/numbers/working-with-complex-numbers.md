---
date: 2024-01-26 04:37:49.477082-07:00
description: "Como fazer: C++ possui uma biblioteca embutida `<complex>` que facilita\
  \ o trabalho com n\xFAmeros complexos. Aqui est\xE1 uma r\xE1pida olhada."
lastmod: '2024-03-13T22:44:46.873723-06:00'
model: gpt-4-0125-preview
summary: "C++ possui uma biblioteca embutida `<complex>` que facilita o trabalho com\
  \ n\xFAmeros complexos."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## Como fazer:
C++ possui uma biblioteca embutida `<complex>` que facilita o trabalho com números complexos. Aqui está uma rápida olhada:

```cpp
#include <iostream>
#include <complex>

int main() {
    std::complex<double> num1(2.0, 3.0); // Cria um número complexo (2 + 3i)
    std::complex<double> num2(3.0, 4.0); // Outro número complexo (3 + 4i)

    // Adição
    std::complex<double> resultado = num1 + num2;
    std::cout << "Resultado da adição: " << resultado << std::endl; // (5 + 7i)

    // Multiplicação
    resultado = num1 * num2;
    std::cout << "Resultado da multiplicação: " << resultado << std::endl; // (-6 + 17i)

    // Conjugado
    resultado = std::conj(num1);
    std::cout << "Conjugado do num1: " << resultado << std::endl; // (2 - 3i)
    
    return 0;
}
```

## Aprofundando
Números complexos têm uma rica história, aparecendo pela primeira vez nas soluções de equações cúbicas no século 16. Eles são essenciais em muitos campos, não apenas em programação. Dentro da ciência da computação, números complexos auxiliam em algoritmos que requerem um espaço numérico bidimensional, como a Transformada Rápida de Fourier (FFT).

Embora a biblioteca `<complex>` do C++ seja padrão, existem alternativas em outras linguagens, como o tipo de dados `complex` do Python ou bibliotecas matemáticas do JavaScript. A biblioteca `<complex>` em si fornece funcionalidades abrangentes, incluindo operações trigonométricas, exponenciais e logarítmicas adaptadas para números complexos.

Ao programar esses números, é fundamental compreender a matemática subjacente para evitar imprecisões e entender operações como a conjugação complexa, que inverte o sinal da parte imaginária, ou as implicações da fórmula de Euler que relaciona exponenciais complexos a funções trigonométricas.

## Veja Também
- A Documentação da Standard Template Library do C++: https://en.cppreference.com/w/cpp/header/complex
- Um mergulho mais profundo na matemática dos números complexos: https://mathworld.wolfram.com/ComplexNumber.html
- Para visualização, a biblioteca Python Matplotlib pode plotar números complexos: https://matplotlib.org/
