---
aliases:
- /pt/kotlin/refactoring/
date: 2024-01-26 01:44:01.959580-07:00
description: "Refatora\xE7\xE3o \xE9 o processo de ajustar o c\xF3digo existente para\
  \ melhorar sua estrutura, legibilidade e desempenho sem alterar seu comportamento\
  \ externo.\u2026"
lastmod: 2024-02-18 23:08:58.117686
model: gpt-4-0125-preview
summary: "Refatora\xE7\xE3o \xE9 o processo de ajustar o c\xF3digo existente para\
  \ melhorar sua estrutura, legibilidade e desempenho sem alterar seu comportamento\
  \ externo.\u2026"
title: "Refatora\xE7\xE3o"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Refatoração é o processo de ajustar o código existente para melhorar sua estrutura, legibilidade e desempenho sem alterar seu comportamento externo. Programadores refatoram para tornar o código mais fácil de manter, simplificar a adição de novos recursos e detectar e corrigir bugs mais facilmente.

## Como Fazer:
Aqui está um trecho em Kotlin mostrando um problema comum no código e sua versão refatorada. Começamos com um trecho de código que está fazendo demais:

```kotlin
fun processOrders(orders: List<Order>) {
    for (order in orders) {
        print("ID do Pedido: ${order.id}")
        // Calculando o total do pedido
        var total = 0.0
        for (item in order.items) {
            total += item.price
        }
        // Aplicar desconto
        if (order.customer.isVIP) {
            total *= 0.9
        }
        print("Total: $total")
        // Mais processamento...
    }
}
```

Refatorado para melhor legibilidade e separação de preocupações:

```kotlin
fun printOrderSummary(order: Order) {
    print("ID do Pedido: ${order.id}")
    val total = calculateTotal(order)
    print("Total: $total")
}

fun calculateTotal(order: Order): Double {
    var total = order.items.sumOf { it.price }
    return if (order.customer.isVIP) total * 0.9 else total
}

fun processOrders(orders: List<Order>) {
    orders.forEach { printOrderSummary(it) }
}
```

Não temos saída de exemplo aqui, pois não mudamos a funcionalidade, mas a legibilidade e a manutenção do código receberam um grande impulso!

## Aprofundando
Refatoração como conceito existe desde o início da programação, mas realmente ganhou força como disciplina nos anos 1990, especialmente após Martin Fowler publicar "Refactoring: Improving the Design of Existing Code" em 1999. Esse livro deu nome à prática e definiu um método organizado para aplicá-la, incluindo um catálogo de técnicas de refatoração.

Comparando a refatoração com alternativas: você poderia reescrever o código do zero (arriscado e demorado), ou simplesmente fazer mudanças aditivas (leva ao inchaço do software e à possível dívida técnica). Refatoração acerta em cheio - moderniza e limpa mantendo o risco baixo.

Em termos de implementação, é essencial ter um conjunto robusto de testes antes de começar a refatorar para garantir que não altere acidentalmente o comportamento do programa. Muitas IDEs modernas (incluindo IntelliJ para Kotlin) têm ferramentas de refatoração automatizadas para renomear variáveis, extrair métodos e mais, o que pode acelerar o processo e reduzir erros.

## Veja Também
- "Refactoring: Improving the Design of Existing Code" por Martin Fowler (pelo trabalho fundamental sobre este tópico)
- Documentação do Kotlin sobre convenções de código: [https://kotlinlang.org/docs/coding-conventions.html](https://kotlinlang.org/docs/coding-conventions.html) (para entender a 'maneira Kotlin' de código limpo)
- Suporte da JetBrains para refatoração no IntelliJ IDEA: [https://www.jetbrains.com/help/idea/refactoring-source-code.html](https://www.jetbrains.com/help/idea/refactoring-source-code.html) (para uso prático de ferramentas de refatoração)
- Guia do Google sobre refatoração em escala: [https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html](https://testing.googleblog.com/2017/06/code-health-to-comment-or-not-to-comment.html) (para insights sobre enfrentar desafios maiores de refatoração)
