---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:10.230636-07:00
description: "Como fazer: Swift torna o trabalho com arrays associativos direto. Veja\
  \ como voc\xEA pode declarar, adicionar, remover e acessar itens em um dicion\xE1\
  rio Swift."
lastmod: '2024-03-13T22:44:46.913828-06:00'
model: gpt-4-0125-preview
summary: Swift torna o trabalho com arrays associativos direto.
title: Usando arrays associativos
weight: 15
---

## Como fazer:
Swift torna o trabalho com arrays associativos direto. Veja como você pode declarar, adicionar, remover e acessar itens em um dicionário Swift:

```Swift
// Declarando um dicionário
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Adicionando um novo item
fruitColors["Grape"] = "Purple"

// Acessando um valor usando sua chave
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // Saída: Apple is Red.
} else {
    print("Color not found.")
}

// Removendo um item
fruitColors["Banana"] = nil  // Isso removerá "Banana" do dicionário

// Iterando sobre os itens
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // Saída:
    // Apple is Red.
    // Grape is Purple.
}
```

Dicionários são incrivelmente versáteis, permitindo a manipulação e acesso dinâmico aos dados. Sua natureza não ordenada não impacta a velocidade de recuperação de dados, o que é um benefício significativo ao lidar com grandes conjuntos de dados.

## Aprofundando
A implementação de dicionários como um array associativo no Swift decorre de sua poderosa capacidade de mapear chaves únicas a valores. Historicamente, as linguagens de programação implementaram esse conceito sob vários nomes como tabelas de hash ou mapas, aludindo à sua funcionalidade de criar um "mapa" entre chaves e valores.

No Swift, os dicionários são otimizados para desempenho, aproveitando chaves que podem ser hasheadas para a recuperação eficiente de dados. Isso significa que o tipo `Key` em um dicionário `[Key: Value]` deve estar em conformidade com o protocolo `Hashable`, o que é o caso para a maioria dos tipos padrão do Swift, como `Int`, `String` e `Double`.

Uma coisa a considerar é que, embora os dicionários sejam excelentes para associar pares de dados, eles não possuem ordem. Se você precisa manter a ordem dos elementos, pode explorar alternativas como `Array` para uma sequência de elementos ordenados ou estruturas de dados personalizadas que combinam os recursos de ambos, arrays e dicionários.

Também é digno de nota que o Swift evolui continuamente, assim como seu manuseio e otimizações de dicionários. Portanto, manter-se atualizado com a documentação mais recente do Swift é crucial para aproveitar ao máximo os dicionários, garantindo que você esteja usando as práticas mais eficientes e atualizadas.
