---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:53.310736-07:00
description: "Como Fazer: Em Rust, o tipo `HashMap` do m\xF3dulo `std::collections`\
  \ fornece a funcionalidade de arrays associativos. Aqui est\xE1 como voc\xEA pode\
  \ trabalhar com\u2026"
lastmod: '2024-03-13T22:44:46.360815-06:00'
model: gpt-4-0125-preview
summary: "Em Rust, o tipo `HashMap` do m\xF3dulo `std::collections` fornece a funcionalidade\
  \ de arrays associativos."
title: Usando arrays associativos
weight: 15
---

## Como Fazer:
Em Rust, o tipo `HashMap` do módulo `std::collections` fornece a funcionalidade de arrays associativos. Aqui está como você pode trabalhar com eles:

```Rust
use std::collections::HashMap;

fn main() {
    // Criando um novo HashMap
    let mut scores = HashMap::new();

    // Inserindo valores
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // Acessando valores
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("Pontuação para o time Blue: {}", score); // Saída: Pontuação para o time Blue: 10
    }

    // Atualizando um valor
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // Iterando sobre pares de chave-valor
    for (key, value) in &scores {
        println!("{}: {}", key, value); // Saída: Blue: 15, Yellow: 50
    }
}
```

## Aprofundamento
O `HashMap` em Rust usa uma função de hashing para mapear chaves a valores, o que permite a recuperação rápida de dados. No entanto, essa eficiência tem um custo: hash maps não mantêm a ordem de seus elementos. Isso contrasta com outras implementações de arrays associativos, como aquelas em Python (`dict`) ou Ruby, que, nas versões mais recentes, mantêm a ordem de inserção como uma característica. Para casos de uso em que a ordem dos pares chave-valor é significativa, desenvolvedores Rust podem considerar o uso do `BTreeMap` do módulo `std::collections`, que mantém a ordem mas pode oferecer inserção e recuperação mais lentas em comparação ao `HashMap`. Em última análise, a escolha entre `HashMap` e `BTreeMap` depende de requisitos específicos em torno de ordenação e desempenho.
