---
title:                "Escrevendo testes"
html_title:           "C#: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## O que é e por que fazer testes de programação?

Testes de programação são um conjunto de ações que os programadores realizam para verificar se o código que eles escreveram está funcionando corretamente. É uma forma de garantir que o software produzido seja de alta qualidade e atenda às expectativas do cliente. Além disso, os testes de programação ajudam a identificar e corrigir erros antes que o software seja lançado, economizando tempo e recursos no longo prazo.

## Como fazer:

```C#
// Criando um teste simples de soma
int resultado = Soma(2, 3);
int esperado = 5;

// Verificando se o resultado é igual ao esperado
if (resultado == esperado) {
  Console.WriteLine("O teste passou!");
} else {
  Console.WriteLine("O teste falhou...");
}

// Função para somar dois números
int Soma(int a, int b) {
  return a + b;
}
```

Saída: O teste passou!

## Profundidade:

Escrever testes de programação não é uma prática nova. Na verdade, ela tem suas raízes na metodologia de desenvolvimento de software conhecida como "Test Driven Development" (TDD), que foi introduzida por Kent Beck na década de 1990. Existem outras alternativas, como "Behavior Driven Development" (BDD) e "Acceptance Test Driven Development" (ATDD), que também enfatizam a importância de testes de programação no processo de desenvolvimento de software.

Para implementar testes em C#, é recomendado o uso de frameworks de testes, como NUnit, xUnit ou MSTest. Eles fornecem ferramentas e estruturas que facilitam a criação e execução de testes automatizados. Além disso, a plataforma Visual Studio possui suporte integrado para criação e execução de testes em C#, tornando o processo ainda mais fácil.

## Veja também:

- [Artigo sobre TDD](https://www.devmedia.com.br/test-driven-development-conceitos/30770)
- [Tutorial de testes com NUnit](https://code.tutsplus.com/pt/tutorials/common-unit-testing-scenarios-with-nunit--net-10458)