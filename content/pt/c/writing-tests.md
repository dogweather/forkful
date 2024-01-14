---
title:                "C: Escrevendo testes"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-tests.md"
---

{{< edit_this_page >}}

Por que escrever testes em C pode melhorar seu código

Escrever testes é uma das melhores práticas de programação que pode ajudar a melhorar a qualidade do seu código em C. Testes bem escritos podem detectar erros e bugs em seu código antes mesmo de serem implementados, economizando tempo e esforço.

Como escrever testes em C

Para escrever testes em C, você deve primeiro garantir que tenha uma compreensão sólida do código que está testando. Em seguida, você deve seguir as etapas abaixo:

1. **Escolha um framework de teste**: existem várias opções de frameworks de teste disponíveis para C, como "CuTest" e "Check". Escolha o que melhor se adequa às suas necessidades e conhecimentos.

2. **Defina suas funções de teste**: crie funções de teste que verifiquem o comportamento de suas funções no código. Essas funções devem retornar "pass" ou "fail" com base nos resultados do teste.

3. **Utilize as macros adequadas**: as macros fornecidas pelo framework de teste selecionado ajudarão a verificar assertivamente os resultados esperados no seu código.

4. **Execute seus testes**: compilo e execute seus testes para verificar se eles estão retornando os resultados esperados. Se alguns testes falharem, você precisará fazer ajustes no seu código.

Exemplo de código em C:

```C
#include <stdio.h> 
#include "CuTest.h" 

void test_function1(CuTest* test) { 
    CuAssertIntEquals(test, 10, function1(2, 5)); 
    CuAssertIntEquals(test, 45, function1(15, 30)); 
} 

void test_function2(CuTest* test) { 
    CuAssertIntEquals(test, 5, function2(10)); 
    CuAssertIntEquals(test, -2, function2(-4)); 
} 

CuSuite* function1_suite() { 
    CuSuite* suite = CuSuiteNew(); 
    SUITE_ADD_TEST(suite, test_function1); 
    return suite; 
} 

CuSuite* function2_suite() { 
    CuSuite* suite = CuSuiteNew(); 
    SUITE_ADD_TEST(suite, test_function2); 
    return suite; 
} 

void all_suites(CuSuite* testSuite) { 
    SUITE_ADD_SUITE(testSuite, function1_suite()); 
    SUITE_ADD_SUITE(testSuite, function2_suite()); 
} 

int main() { 
    CuSuite *suites = CuSuiteNew(); 
    all_suites(suites); 
    CuSuiteRun(suites); 
    
    printf("Number of tests run: %d\n", suites->countRun); 
    
    CuString *output = CuStringNew(); 
    CuSuiteSummary(suites, output); 
    printf("Summary: %s\n", output->buffer); 
    
	CuSuiteDetails(suites, output); 
    printf("Details: %s/n", output->buffer); 
    
    CuSuiteDelete(suites); 
    CuStringDelete(output); 
    return 0; 
}

```

Saída esperada:

```
Number of tests run: 4 
Summary: Cases run: 4, Failures: 0 
Details: Test function1 <PASSED> 
Test function2 <PASSED> 

See Also

Para saber mais sobre como escrever testes em C, confira os links abaixo:

- [Tutorial: How to write test cases in C](https://www.tutorialspoint.com/cprogramming/cprogramming_unittesting.htm)
- [Using Check: A unit testing framework for C](https://libcheck.github.io/check/)
- [Unit Testing Techniques for C](https://www.slideshare.net/GeoffreySereg/testing-techniques-for-c)