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

## O que é e por que fazer testes?

Escrever testes em programação significa criar pequenos programas que testam cada parte do seu código para garantir que tudo funcione corretamente. Os programadores fazem isso para se certificar de que suas alterações ou novos recursos não quebram o código existente e para evitar erros antes de entregá-lo aos usuários.

## Como fazer:

```
C
#include <stdio.h>

// função que retorna a soma de dois números
int somar(int a, int b) {
  return a + b;
}

// função de teste para a função somar
void testar_somar() {
  int resultado = somar(2, 3);
  if (resultado == 5) {
    printf("TESTE PASSOU: 2 + 3 = 5");
  } else {
    printf("TESTE FALHOU: 2 + 3 = %d", resultado);
  }
}

int main() {
  // chamando a função de teste
  testar_somar();
  return 0;
}
```

Output:
```
TESTE PASSOU: 2 + 3 = 5
```

## Aprofundando:

Testes de unidade, também conhecidos como testes de caixa branca, se tornaram populares com o desenvolvimento de metodologias ágeis de desenvolvimento de software. Eles ajudam a garantir que o código seja confiável e de alta qualidade. Existem outras formas de testes, como testes de integração e testes de aceitação, mas o foco dos testes de unidade é em partes individuais do código.

## Veja também:

- [Artigo sobre testes de unidade na Wikipedia](https://pt.wikipedia.org/wiki/Teste_de_unidade)
- [Livro sobre testes de software: "Art of Unit Testing"](https://www.amazon.com.br/Art-Unit-Testing-Examples/dp/1617290890)
- [Ferramenta de testes em C: CUnit](http://cunit.sourceforge.net/)