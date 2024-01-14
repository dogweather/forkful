---
title:    "C: Escrevendo testes"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes é uma prática essencial para garantir a qualidade e a estabilidade de um código em C. Testes bem escritos podem ajudar a encontrar e corrigir erros antes mesmo deles impactarem um programa em produção. Além disso, testes bem estruturados podem funcionar como uma documentação viva do código, facilitando o entendimento e a manutenção do mesmo.

## Como escrever testes em C?

Para escrever testes em C, é necessário utilizar uma biblioteca de testes, como a "cmocka", por exemplo. Esta biblioteca fornece funções e macros para facilitar a escrita e execução de testes.

Um exemplo de teste utilizando a biblioteca "cmocka" seria o seguinte:

```C
#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

// Função a ser testada
int somar(int a, int b){
    return a + b;
}

// Exemplo de teste
void teste_somar(){
    assert_int_equal(somar(2,3), 5); // Verifica se a soma de dois números é igual ao esperado
}

// Função main para rodar os testes
int main(){
    const struct CMUnitTest tests[] = {
        cmocka_unit_test(teste_somar), // Adiciona o teste criado à lista de testes
    };
    
    return cmocka_run_group_tests(tests, NULL, NULL); // Executa os testes e retorna o resultado
}
```

Este é apenas um exemplo simples de como escrever testes utilizando a biblioteca "cmocka". Existem outras bibliotecas e formas de escrever testes em C, porém o conceito básico é o mesmo: criar funções de teste que verifiquem se o código se comporta como esperado em diferentes situações.

## Aprofundando nos testes em C

Uma boa prática ao escrever testes em C é seguir o padrão "Given-When-Then", que significa "Dado-Quando-Então", em tradução livre. Neste padrão, o teste é dividido em três partes: a preparação do código (Given), execução da função a ser testada (When) e verificação dos resultados (Then). Isso ajuda a manter os testes organizados e fáceis de entender.

Outra dica importante é testar tanto os casos positivos (quando o código deve funcionar corretamente) quanto os casos negativos (quando o código deve falhar). Isso garante uma cobertura maior dos testes e ajuda a encontrar possíveis erros em situações inesperadas.

Além disso, é importante lembrar de manter os testes atualizados sempre que for feita alguma mudança no código. Caso contrário, os testes podem se tornar obsoletos e não cumprir mais seu papel de garantir a qualidade do código.

## Veja também

- [Documentação da biblioteca "cmocka"](https://cmocka.org/)
- [Outras bibliotecas de testes em C](https://www.toolsie.com/2017/08/28/CppUnit-GTest-CMocka-UNITY-example/)
- [Explicação detalhada do padrão "Given-When-Then"](https://blog.novoda.com/code-quality-tdd-flow-8/)