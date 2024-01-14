---
title:                "C: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante para programadores

Escrever testes é uma parte essencial do processo de desenvolvimento de software, especialmente na programação em C. Esse tipo de linguagem requer uma abordagem minuciosa e cuidadosa, pois pequenos erros podem se tornar grandes problemas no funcionamento do programa. Escrever testes ajuda a garantir que o código esteja funcionando corretamente e a detectar possíveis erros antes que eles causem falhas críticas no sistema.

## Como escrever testes em C

Escrever testes em C pode parecer intimidante no início, mas com um pouco de prática, se torna uma tarefa simples e eficiente. Primeiramente, é importante escolher um framework de testes, como o CUnit ou Unity, para facilitar a organização e execução dos testes.

A seguir, vamos ver um exemplo de código em C que utiliza o framework CUnit para escrever e executar testes em uma função que realiza a soma de dois números inteiros:

```C
#include <CUnit/CUnit.h>

// Função a ser testada
int soma(int a, int b) {
    return a + b;
}

// Função auxiliar para inicializar os testes
int iniciar_testes() {
    return 0;
}

// Função auxiliar para encerrar os testes
int encerrar_testes() {
    return 0;
}

// Teste para verificar se a função soma está retornando o valor correto
void teste_soma() {
    CU_ASSERT_EQUAL(soma(2, 3), 5); // 2 + 3 = 5
    CU_ASSERT_EQUAL(soma(10, -5), 5); // 10 + (-5) = 5
    CU_ASSERT_EQUAL(soma(-8, -8), -16); // (-8) + (-8) = -16
}

// Adiciona os testes a uma suite e registra as funções auxiliares
void adicionar_testes() {
    CU_pSuite suite = CU_add_suite("Suite de testes para soma", iniciar_testes, encerrar_testes);
    CU_add_test(suite, "teste_soma", teste_soma);
}

// Função principal para executar os testes
int main() {
    CU_initialize_registry();
    adicionar_testes();

    CU_basic_run_tests();
    CU_cleanup_registry();

    return 0;
}
```

Neste exemplo, utilizamos a função `CU_ASSERT_EQUAL` para comparar o resultado da função `soma` com o valor esperado. Ao executar o programa, se algum dos testes falhar, o framework CUnit irá mostrar uma mensagem de erro indicando qual teste falhou e o porquê.

## Uma visão mais profunda sobre escrita de testes

Escrever testes é uma prática importante para garantir a qualidade e eficiência do seu código, mas também pode ajudar a melhorar o design e a organização do seu programa. Ao escrever os testes antes de implementar a função, você pode pensar nos possíveis casos de uso e no comportamento esperado da função, o que pode levar a um código mais claro e legível.

Além disso, escrever testes pode ser útil para evitar regressões em seu código. Caso você precise fazer alterações em uma função já existente, ter testes para essa função irá garantir que o seu novo código não quebre a funcionalidade anterior.

## Veja também
- [Documentação do CUnit](https://github.com/jfoster/cunit)
- [Framework de testes Unity para C](https://github.com/throwtheswitch/unity)
- [Tutorial de escrita de testes em C](https://www.skybert.net/c/unit-testing-c-code/)
- [Exemplos de testes em C](https://github.com/chriskohlhoff/CxxSpec)
- [Guia de boas práticas para escrita de testes em C](https://github.com/Leo230/ctests/blob/master/doc/guide_pt_br.md)