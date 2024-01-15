---
title:                "Escrevendo testes"
html_title:           "C: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-tests.md"
---

{{< edit_this_page >}}

## Por que

Se você é um programador em C, provavelmente já ouviu falar sobre testes de código. Mas por que escrever testes é tão importante? Bem, para começar, testes são uma forma de garantir que o seu código funciona corretamente e continua funcionando conforme você o modifica. Além disso, eles ajudam a identificar e corrigir erros antes que o código seja enviado para produção, o que economiza tempo e recursos.

## Como fazer

Para escrever testes em C, você pode usar os frameworks de teste populares, como o Unity ou o Google Test. Esses frameworks fornecem funções e macros específicos para criar testes simples e eficazes. Por exemplo, vamos supor que você tem uma função que calcula o fatorial de um número inteiro:

```C
int fatorial(int n) {
  if (n < 0) {
    return -1;
  } else if (n == 0) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}
```

Para testar essa função, podemos usar o framework Unity para criar um teste simples que verifica se o resultado do fatorial de 5 é igual a 120:

```C
#include "unity.h"

void test_fatorial() {
    int resultado = fatorial(5);
    TEST_ASSERT_EQUAL(120, resultado);
}
```

Se o valor retornado pela função `fatorial()` for igual a 120, o teste será considerado bem-sucedido. Caso contrário, o teste falhará e o resultado será exibido junto com uma mensagem indicando o motivo do erro.

## Profundando

Agora que você já sabe como escrever testes em C, é importante entender alguns conceitos fundamentais para garantir a eficácia dos seus testes. Primeiro, é importante ter uma boa cobertura de testes, ou seja, ter testes para todas as partes do seu código. Além disso, os testes devem ser independentes, o que significa que eles não devem depender uns dos outros para serem executados.

Outro conceito importante é o TDD (Desenvolvimento Orientado a Testes), que sugere escrever os testes antes mesmo de escrever o código. Isso garante que o código seja escrito com foco em sua funcionalidade e não apenas para fazer os testes passarem.

Por fim, lembre-se de sempre revisar e atualizar seus testes à medida que o código evolui. Isso garantirá que seus testes continuem eficazes e que seu código permaneça livre de bugs.

## Veja também

- [Documentação do Unity para C](https://github.com/ThrowTheSwitch/Unity)
- [Tutorial de testes em C com Unity](https://embeddedartistry.com/blog/2019/03/13/unit-testing-c-code-with-unity/)
- [TDD em C com CppUTest](https://cpputest.github.io/)