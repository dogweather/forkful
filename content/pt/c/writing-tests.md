---
title:                "Escrevendo testes"
date:                  2024-01-19
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c/writing-tests.md"
---

{{< edit_this_page >}}

## O que é & Por quê?
Escrever testes é o processo de checar se o seu código faz exatamente aquilo que você espera que ele faça. Programadores testam para evitar bugs, garantir a qualidade do software e facilitar manutenções futuras.

## Como fazer:
```C
#include <assert.h>

// Função para testar
int soma(int a, int b) {
    return a + b;
}

// Teste da função
void testeSoma() {
    assert(soma(2, 2) == 4);
    assert(soma(-1, 1) == 0);
}

int main() {
    testeSoma();
    printf("Todos os testes passaram!\n");
    return 0;
}
```
Saída esperada:
```
Todos os testes passaram!
```

## Aprofundando
Historicamente, o teste de software começou como um processo manual, mas evoluiu para incluir testes automáticos e frameworks especializados. Alternativas modernas em C incluem frameworks como CUnit, Check e Unity. Ao escrever testes, é crucial considerar casos de borda, verificar tanto a correção quanto o desempenho e manter os testes atualizados conforme o código evolui.

## Veja também
- Documentação do CUnit: https://cunit.sourceforge.io/doc/index.html
- Check: https://libcheck.github.io/check/
- Unity Test API: http://www.throwtheswitch.org/unity
- Artigo sobre Test-Driven Development (TDD): https://en.wikipedia.org/wiki/Test-driven_development
