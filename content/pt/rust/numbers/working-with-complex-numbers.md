---
date: 2024-01-26 04:45:22.661164-07:00
description: "N\xFAmeros complexos possuem uma parte real e uma parte imagin\xE1ria\
  \ e s\xE3o cruciais em diversas \xE1reas, como engenharia, f\xEDsica e gr\xE1ficos\
  \ de computador.\u2026"
lastmod: '2024-03-13T22:44:46.361782-06:00'
model: gpt-4-0125-preview
summary: "N\xFAmeros complexos possuem uma parte real e uma parte imagin\xE1ria e\
  \ s\xE3o cruciais em diversas \xE1reas, como engenharia, f\xEDsica e gr\xE1ficos\
  \ de computador."
title: "Trabalhando com n\xFAmeros complexos"
weight: 14
---

## O Que & Por Quê?
Números complexos possuem uma parte real e uma parte imaginária e são cruciais em diversas áreas, como engenharia, física e gráficos de computador. Programadores os usam para resolver equações que números reais comuns não conseguem.

## Como fazer:
Rust não tem suporte embutido para números complexos, mas crates como `num-complex` estão aqui para ajudar. Veja como usar:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let soma = a + b;
    let produto = a * b;

    println!("Soma: {}", soma); // Soma: 3 - 1i
    println!("Produto: {}", produto); // Produto: 14 - 5i
}
```
Você precisará adicionar `num_complex` ao seu `Cargo.toml` para fazer essa mágica acontecer.

## Mergulho Profundo
Números complexos foram concebidos no século 16, mas realmente decolaram no século 18, quando matemáticos como Euler começaram a brincar com eles.

Sem operações nativas de números complexos, linguagens como Rust dependem de bibliotecas de terceiros. `num-complex` é uma dessas crates e faz parte da coleção de crates `num`, que visa fornecer tipos e traits numéricos para Rust.

Vale mencionar que algumas linguagens (como Python) têm suporte embutido para números complexos, enquanto outras (como C++, com o cabeçalho `<complex>`) os fornecem como parte da biblioteca padrão. Em Rust, a decisão de manter a biblioteca padrão pequena significa que você frequentemente recorrerá a crates criadas pela comunidade para funcionalidades adicionais.

## Veja Também
- [Livro do Rust](https://doc.rust-lang.org/book/): Para aprender mais sobre Rust e como trabalhar com crates externas.
- [Número Complexo na Wikipedia](https://pt.wikipedia.org/wiki/N%C3%BAmero_complexo): Para um entendimento mais profundo sobre números complexos.
