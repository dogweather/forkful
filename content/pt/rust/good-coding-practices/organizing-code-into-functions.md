---
date: 2024-01-26 01:11:40.816201-07:00
description: "Como fazer: Imagine que voc\xEA tem um c\xF3digo que calcula a \xE1\
  rea de um c\xEDrculo v\xE1rias vezes. Em vez de repetir a f\xF3rmula, voc\xEA a\
  \ encapsula em uma fun\xE7\xE3o."
lastmod: '2024-03-13T22:44:46.373081-06:00'
model: gpt-4-1106-preview
summary: "Imagine que voc\xEA tem um c\xF3digo que calcula a \xE1rea de um c\xEDrculo\
  \ v\xE1rias vezes."
title: "Organizando o c\xF3digo em fun\xE7\xF5es"
weight: 18
---

## Como fazer:
Imagine que você tem um código que calcula a área de um círculo várias vezes. Em vez de repetir a fórmula, você a encapsula em uma função.

```Rust
fn calcular_area_circulo(raio: f64) -> f64 {
    std::f64::consts::PI * raio.powi(2)
}

fn main() {
    let raio = 5.0;
    let area = calcular_area_circulo(raio);
    println!("A área do círculo é: {}", area);
}
```

Saída:

```
A área do círculo é: 78.53981633974483
```

## Aprofundamento
Historicamente, as funções vêm da matemática, onde mapeiam entradas para saídas. Na programação, elas existem desde os tempos de assembly, embora as chamássemos de 'subrotinas'. As funções Rust podem retornar valores e até outras funções, graças às funções de primeira classe e closures.

Alternativas? Código embutido ou macros, mas eles são confusos para lógicas complexas. Objetos com métodos são outro modo de organizar funcionalidades, um sabor diferente das funções autônomas.

A implementação em Rust é bem direta. As funções declaram os tipos dos seus parâmetros e o tipo de retorno. Elas utilizam 'snake case' para a nomenclatura por convenção. Você tem suas funções públicas (`pub fn`) para uso fora do módulo e privadas para uso interno. E o Rust tem esse recurso bacana onde você não precisa da palavra-chave `return` para a última expressão em uma função.

## Veja Também
Confira estes para mais informações:
- The Rust Programming Language Book: [Funções](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust por Exemplo em [Funções](https://doc.rust-lang.org/rust-by-example/fn.html)
