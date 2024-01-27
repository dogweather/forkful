---
title:                "Escrevendo testes"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Escrever testes é criar códigos específicos que verificam se outras partes do programa operam corretamente. Programadores realizam testes para garantir que o código funcione como esperado, para prevenir bugs e para facilitar a manutenção e evolução do software.

## Como fazer:

```Rust
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn outra_funcao() {
        assert!(1 == 1);
    }

    #[test]
    #[should_panic(expected = "divisão por zero")]
    fn falha_na_divisao() {
        // Isto deve causar uma falha!
        let _resultado = 1 / 0;
    }
}
```

Saída esperada no terminal ao executar `cargo test`:

```
running 2 tests
test tests::outra_funcao ... ok
test tests::it_works ... ok

test result: ok. 2 passed; 0 failed; 0 ignored; 0 measured; 1 filtered out

running 1 test
test tests::falha_na_divisao ... FAILED

test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out
```

## Mergulho Profundo

Testes automáticos em Rust são a norma desde a primeira versão da linguagem. Alternativas incluem teste de integração e testes de documentação com cargas mais pesadas de contexto. Para testes unitários, Rust utiliza anotações para identificar testes e macros `assert!`, `assert_eq!`, e `assert_ne!` para fazer afirmações. Rust permite a execução de testes em paralelo e testes condicionais com a flag `#[cfg(test)]`.

## Veja Também

- Rust Book sobre testes: https://doc.rust-lang.org/book/ch11-00-testing.html
- Cargo Book sobre testes: https://doc.rust-lang.org/cargo/guide/tests.html
- Rust por exemplo (Rust by Example) sobre testes: https://doc.rust-lang.org/rust-by-example/testing.html
