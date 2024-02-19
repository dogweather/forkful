---
aliases:
- /pt/c/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:37.375992-07:00
description: "Escrever testes em C envolve criar programas ou fun\xE7\xF5es auxiliares\
  \ menores que verificam automaticamente a funcionalidade do seu c\xF3digo. Os programadores\u2026"
lastmod: 2024-02-18 23:08:58.623215
model: gpt-4-0125-preview
summary: "Escrever testes em C envolve criar programas ou fun\xE7\xF5es auxiliares\
  \ menores que verificam automaticamente a funcionalidade do seu c\xF3digo. Os programadores\u2026"
title: Escrevendo testes
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Escrever testes em C envolve criar programas ou funções auxiliares menores que verificam automaticamente a funcionalidade do seu código. Os programadores fazem isso para garantir que seu software funcione conforme esperado, para capturar bugs precocemente e facilitar futuras modificações no código sem efeitos colaterais indesejados.

## Como fazer:
Embora o C não tenha uma framework de testes integrada como algumas outras linguagens, ainda é possível escrever testes eficazes usando assert.h para asserções simples ou integrar frameworks de terceiros como CUnit ou Unity para testes mais estruturados. Aqui está um exemplo básico usando assert.h para testar uma função que adiciona dois inteiros:

```c
#include <assert.h>
#include "my_math.h"

void test_addition() {
    assert(add(1, 2) == 3);
    assert(add(-1, -2) == -3);
    assert(add(0, 0) == 0);
    printf("Todos os testes de adição passaram.\n");
}

int main() {
    test_addition();
    return 0;
}
```

Em `my_math.h`, você poderia ter:

```c
// Função simples de adição
int add(int a, int b) {
    return a + b;
}
```

Executar a função de teste na sua função `main` exibe:

```
Todos os testes de adição passaram.
```

Para uma configuração de teste mais abrangente usando uma framework como Unity, você incorporaria a framework ao seu projeto, e então escreveria casos de teste de maneira similar, mas utilizando a API da framework para asserções e execução de testes.

## Aprofundando
Testar em C historicamente foi um processo manual e um tanto quanto ad hoc devido à natureza de baixo nível da linguagem e à falta de uma framework de testes padronizada. Esta abordagem manual muitas vezes levava a práticas de teste menos rigorosas comparadas às linguagens com suporte integrado para testes. Como a linguagem C tem sido crucial no desenvolvimento de sistemas de software fundamentais, essa falta de frameworks de teste formais incentivou a comunidade C a desenvolver soluções de terceiros, como CUnit e Unity.

Essas ferramentas, apesar de externas à biblioteca padrão do C, fornecem funcionalidades semelhantes às frameworks de teste de outras linguagens, oferecendo uma maneira estruturada de definir, executar e avaliar testes. Elas ajudam a preencher a lacuna entre o poderoso acesso ao sistema do C e a prática moderna de desenvolvimento de testes automatizados. Vale ressaltar que, embora essas ferramentas aprimorem significativamente o processo de teste em C, elas podem introduzir uma curva de aprendizado e aumentar a complexidade da configuração do projeto em comparação a linguagens com suporte integrado para testes. Assim, para projetos onde a confiabilidade e a manutenibilidade são primordiais, o investimento na configuração de um ambiente de teste adequado em C é bem justificado, mesmo frente às possíveis alternativas.
