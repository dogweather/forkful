---
title:                "Refatoração"
date:                  2024-01-26T01:18:12.099815-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refatoração"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/refactoring.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Refatoração é o processo de retrabalhar o seu código para torná-lo mais limpo, mais fácil de manter, sem alterar o seu comportamento externo. Programadores refatoram para melhorar a legibilidade, reduzir a complexidade e tornar a base de código mais propícia a futuras atualizações ou adições de recursos.

## Como fazer:
Vamos dizer que você tem um bloco de código onde está fazendo alguns cálculos repetidos ou manipulações de string em várias funções. Esse é um alvo primordial para refatoração. Aqui está um antes e depois usando Gleam, que tem uma forte ênfase em segurança de tipo e imutabilidade:

```gleam
// Antes da refatoração
pub fn calcular_area(largura: Int, altura: Int) -> Int {
  largura * altura
}

pub fn imprimir_area(largura: Int, altura: Int) {
  let area = calcular_area(largura, altura)
  io.println("A área é \(area)")
}

// Depois da refatoração
pub fn calcular_area(largura: Int, altura: Int) -> Int {
  largura * altura
}

pub fn imprimir_area(area: Int) {
  io.println("A área é \(area)")
}

// Em outra parte do seu código, você chamará imprimir_area assim:
imprimir_area(calcular_area(10, 20))
```

Exemplo de saída:
```
A área é 200
```

Por meio da refatoração, tornamos `imprimir_area` mais focada apenas em imprimir, enquanto o cálculo é tratado em outro lugar, tornando o código mais modular e mais fácil de reutilizar ou testar.

## Aprofundamento
Refatoração, como conceito, existe tanto quanto a programação em si—revisitar e limpar o código faz parte de uma boa gestão de casa. A formalização moderna da refatoração, junto com muitas das técnicas e padrões usados hoje, pode ser rastreada até o livro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code" publicado em 1999.

No ecossistema Gleam, a refatoração tem considerações específicas. Uma das mais significativas é a forte verificação de tipo em tempo de compilação, que pode ajudar a pegar erros cedo quando você está movendo as coisas de lugar. As características de correspondência de padrões e imutabilidade do Gleam também podem guiá-lo a escrever um código mais claro e conciso—um dos objetivos primários da refatoração.

Alternativas à refatoração podem incluir reescrever o código do zero ou remendar o código com correções rápidas. No entanto, a refatoração geralmente é a abordagem mais segura e mais eficiente para melhorar o código existente sem introduzir novos bugs, pois envolve transformações incrementais, bem fundamentadas e que preservam o comportamento.

## Veja Também
- O livro "Refactoring" de Martin Fowler: https://martinfowler.com/books/refactoring.html
- O site da linguagem Gleam, com documentação adicional e exemplos: https://gleam.run/
- "Refactoring: Improving the Design of Existing Code" por Martin Fowler (para princípios subjacentes aplicáveis em diversas linguagens): https://martinfowler.com/books/refactoring.html