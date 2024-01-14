---
title:    "Rust: Saida de depuração de impressão"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que usar impressão de saída de depuração em Rust?

A impressão de saída de depuração é uma ferramenta útil para visualizar os dados em um programa Rust. Ela pode ser usada para entender como o código está funcionando e identificar possíveis erros. Além disso, a impressão de saída de depuração é uma forma rápida e fácil de inspecionar variáveis e valores durante a execução do código.

## Como usar a impressão de saída de depuração em Rust

A impressão de saída de depuração é feita através da função `println!()`. Esta função é semelhante ao `println!()` normal, mas adiciona um ponto de exclamação no final para denotar que é uma função de macro. Aqui está um exemplo de como usá-la:

```rust
let nome = "João";
let idade = 25;
println!("Olá, eu sou {} e tenho {} anos.", nome, idade);
```
**Saída:**
> Olá, eu sou João e tenho 25 anos.

## Aprofundando na impressão de saída de depuração em Rust

Além do uso básico da `println!()`, há várias outras opções úteis para imprimir informações de depuração em Rust. Uma das mais comuns é a `dbg!()` que imprime tanto o valor quanto o nome da variável correspondente. Isso pode ser útil para saber em qual momento do código a impressão de saída está sendo executada.

Outra opção é a `eprintln!()` que funciona da mesma forma que a `println!()`, mas imprime a saída na saída de erro padrão, em vez do padrão. Isso pode ser útil para diferenciar entre as diferentes fontes de saída e depuração.

Finalmente, vale mencionar que a macro `format!()` pode ser usada em conjunto com a função `println!()` para formar saídas mais complexas e personalizadas. Aqui está um exemplo:

```rust
let nome = "Maria";
let idade = 30;
println!("Olá, eu sou {} e tenho {} anos. Meu nome tem {} letras.", nome, idade, nome.len());
```

**Saída:**
> Olá, eu sou Maria e tenho 30 anos. Meu nome tem 5 letras.

## Veja também

- [Documentação oficial do Rust sobre impressão de saída de depuração](https://doc.rust-lang.org/std/macro.dbg.html)
- [Vídeo tutorial sobre impressão de saída de depuração em Rust (em inglês)](https://www.youtube.com/watch?v=GNfogT78AOQ)
- [Artigo sobre dicas de depuração em Rust (em inglês)](https://thomascountz.com/2016/11/22/rust-debugging-tips-for-beginners/)