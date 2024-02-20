---
date: 2024-01-26 00:49:58.639415-07:00
description: "Tratar erros significa planejar para quando as coisas derem errado.\
  \ \xC9 vital por ajudar a evitar falhas e tornar seu software robusto e amig\xE1\
  vel para o\u2026"
lastmod: 2024-02-19 22:05:05.947535
model: gpt-4-1106-preview
summary: "Tratar erros significa planejar para quando as coisas derem errado. \xC9\
  \ vital por ajudar a evitar falhas e tornar seu software robusto e amig\xE1vel para\
  \ o\u2026"
title: Tratamento de erros
---

{{< edit_this_page >}}

## O Que & Por Quê?
Tratar erros significa planejar para quando as coisas derem errado. É vital por ajudar a evitar falhas e tornar seu software robusto e amigável para o usuário.

## Como fazer:
Aqui está um bloco básico try-catch para tratar uma exceção:

```cpp
#include <iostream>
#include <stdexcept>

int main() {
    try {
        throw std::runtime_error("Ops! Algo deu errado.");
    } catch (const std::exception& e) {
        std::cerr << "Erro: " << e.what() << std::endl;
    }
    return 0;
}
```

Saída de exemplo:
```
Erro: Ops! Algo deu errado.
```

## Aprofundamento
C++ tem tratamento de erros desde seus primeiros dias. A forma mais básica era verificar os valores de retorno. Se você tem experiência, deve se lembrar dos dias pré-padrão: C com classes e verificação de erros manual.

Depois vieram as exceções com C++ para nos dar uma maneira estruturada de lidar com problemas inesperados. Uma exceção é lançada com `throw` e capturada com `try/catch`.

Dois tipos de erros costumam aparecer: erros lógicos, como um cálculo errado, e erros de execução, como acessar um endereço de memória inválido. Exceções são ideais para erros de execução. Para erros lógicos, é frequentemente melhor usar asserções ou códigos de erro.

Há um debate contínuo sobre exceções versus códigos de erro. Exceções podem ser mais lentas e podem levar a fluxos de controle complexos. Códigos de erro, embora mais rápidos, podem tornar o código confuso e mais difícil de manter. É uma questão de compensação, então conhecer o seu caso de uso é fundamental.

O C++17 introduziu `std::optional` e `std::variant`, que são alternativas às exceções. São úteis para funções que podem ou não retornar um resultado válido.

A segurança de exceções pode ser outra dor de cabeça. Trata-se das garantias que seu código oferece apesar das exceções. Existem três níveis: básico, forte e nothrow. Quanto mais garantias, mais complexo pode ser o seu código.

Pensamentos finais — o tratamento de erros é tanto arte quanto ciência. Ele molda como sua aplicação sobrevive no mundo real. Não abuse das exceções. Mire em um código legível e que possa ser mantido.

## Veja Também
- [cppreference sobre tratamento de exceções](https://en.cppreference.com/w/cpp/language/exceptions)
- [Visão de Bjarne Stroustrup sobre tratamento de erros](http://www.stroustrup.com/except.pdf)
- [Diretrizes do núcleo de C++ sobre exceções](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#Re-exceptions)
