---
title:                "Escrevendo testes"
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?

Escrever testes é criar um conjunto de códigos que executa seu programa para conferir se tudo funciona como deveria. Programadores testam para evitar bugs, garantir a qualidade, e facilitar manutenções futuras.

## How To:

```c++
#include <iostream>
#include <cassert>

void somaTeste() {
    assert(2 + 2 == 4);
    std::cout << "Teste de soma PASSOU!" << std::endl;
}

void subtracaoTeste() {
    assert(2 - 2 == 0);
    std::cout << "Teste de subtracao PASSOU!" << std::endl;
}

int main() {
    somaTeste();
    subtracaoTeste();
    return 0;
}
```
Output:
```
Teste de soma PASSOU!
Teste de subtracao PASSOU!
```

## Aprofundamento

Testes automáticos são a prática padrão desde os anos 2000 com o advento de metodologias ágeis. Alternativas aos `asserts` incluem frameworks mais robustos como Google Test ou Boost.Test, que oferecem mais controle e funcionalidades. A implementação de testes varia: pode ser de unidade, integração, sistema, etc., dependendo do que você precisa avaliar.

## Consulte Também

- Documentação C++ assert: [http://www.cplusplus.com/reference/cassert/assert/](http://www.cplusplus.com/reference/cassert/assert/)
- Google Test GitHub: [https://github.com/google/googletest](https://github.com/google/googletest)
- Boost.Test Documentation: [https://www.boost.org/doc/libs/1_75_0/libs/test/doc/html/index.html](https://www.boost.org/doc/libs/1_75_0/libs/test/doc/html/index.html)
