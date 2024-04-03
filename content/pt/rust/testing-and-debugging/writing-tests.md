---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:10.772977-07:00
description: "Como fazer: O framework de teste embutido do Rust suporta testes de\
  \ unidade, integra\xE7\xE3o e documenta\xE7\xE3o sem a necessidade de bibliotecas\
  \ externas. Os\u2026"
lastmod: '2024-03-13T22:44:46.371177-06:00'
model: gpt-4-0125-preview
summary: "O framework de teste embutido do Rust suporta testes de unidade, integra\xE7\
  \xE3o e documenta\xE7\xE3o sem a necessidade de bibliotecas externas."
title: Escrevendo testes
weight: 36
---

## Como fazer:
O framework de teste embutido do Rust suporta testes de unidade, integração e documentação sem a necessidade de bibliotecas externas. Os testes são anotados com `#[test]`, e qualquer função assim anotada é compilada como um teste.

### Escrevendo um Teste de Unidade:
Coloque os testes de unidade no módulo que estão testando usando um sub-módulo `tests` marcado com `#[cfg(test)]` para garantir que eles sejam compilados apenas quando estiver testando.

```rust
// lib.rs ou main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Executando testes:
```shell
$ cargo test
```

Saída:
```shell
   Compilando seu_nome_de_pacote v0.1.0 (/caminho/para/seu_pacote)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (ou src/main.rs)

executando 1 teste
test tests::it_adds_two ... ok

resultado do teste: ok. 1 passou; 0 falhou; 0 ignorado; 0 medido; 0 filtrado
```

### Escrevendo Testes de Integração:
Testes de integração vão em um diretório de testes no nível superior do seu projeto, ao lado de `src`. Cada arquivo `.rs` em `tests` é compilado como sua própria crate separada.

```rust
// tests/integration_test.rs
use seu_nome_de_pacote;

#[test]
fn it_adds_two() {
    assert_eq!(seu_nome_de_pacote::add(2, 2), 4);
}
```

### Testando com Bibliotecas de Terceiros Populares:
Para capacidades de teste mais extensivas, a biblioteca `proptest` pode gerar uma ampla gama de entradas para testar funções.

Adicione `proptest` como uma dependência de desenvolvimento no `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Use `proptest` para executar o mesmo teste com muitas entradas geradas automaticamente:

```rust
// dentro de tests/integration_test.rs ou um módulo #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        seu_nome_de_pacote::add(a, b);
    }
}
```

Isso verifica que `add` não entra em pânico para uma ampla gama de entradas `i32`.
