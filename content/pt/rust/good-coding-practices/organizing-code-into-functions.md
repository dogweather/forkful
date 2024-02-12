---
title:                "Organizando o código em funções"
aliases:
- /pt/rust/organizing-code-into-functions.md
date:                  2024-01-26T01:11:40.816201-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organizando o código em funções"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?
Organizar o código em funções trata de dividir seu programa em partes reutilizáveis e modulares, identificadas por um nome. Fazemos isso para tornar nosso código mais limpo, legível e fácil de depurar. Trata-se de não repetir a nós mesmos e simplificar atualizações.

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
