---
title:    "Rust: Escrevendo testes"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes em Rust é importante

Escrever testes é uma parte essencial do processo de desenvolvimento de software. Além de ajudar a identificar e corrigir bugs, os testes também garantem que o nosso código funcione corretamente em diferentes situações e cenários. Em Rust, os testes são especialmente importantes porque a linguagem possui um sistema de tipos fortes e enfatiza a segurança e a previsibilidade.

## Como escrever testes em Rust

Para escrever testes em Rust, utilizamos a macro `assert!` que verifica se uma expressão é verdadeira. Vamos supor que queremos testar uma função `soma` que recebe dois números e retorna a soma deles. Nosso teste ficaria assim:

```Rust
fn soma(a: i32, b: i32) -> i32 {
    a + b
}

#[test]
fn test_soma() {
    assert!(soma(1, 2) == 3);
}
```

Ao rodar o teste com o comando `cargo test`, o resultado esperado será "ok", indicando que a expressão é verdadeira. Caso contrário, se houver um erro, o resultado será "fail" e uma mensagem de erro será exibida.

## Mergulho profundo em escrever testes em Rust

Na verdade, o exemplo anterior é bastante simples e talvez não reflita a complexidade dos testes em um projeto real. Em Rust, podemos escrever testes mais elaborados utilizando a macro `assert_eq!` que verifica se dois valores são iguais e também a macro `assert_ne!` que verifica se dois valores são diferentes.

Além disso, em Rust é possível escrever testes modulares, organizando-os em módulos separados e importando-os no arquivo `main.rs`. Também podemos utilizar as annotations `#[ignore]` e `#[should_panic]` para ignorar ou forçar a ocorrência de um pânico durante o teste, respectivamente.

Existem muitas outras funcionalidades e ferramentas disponíveis para testes em Rust, então é importante continuar pesquisando e explorando essas possibilidades para garantir que estamos escrevendo testes eficientes e completos em nossos projetos.

## Veja também

- Documentação oficial sobre testes em Rust: [https://doc.rust-lang.org/rust-by-example/testing.html#unit-tests](https://doc.rust-lang.org/rust-by-example/testing.html#unit-tests)
- Tutorial sobre testes em Rust do site Rustaceans: [https://rustaceans.org/](https://rustaceans.org/)
- Vídeo tutorial sobre testes em Rust do canal Fireship: [https://www.youtube.com/watch?v=bzA5aETuIAE](https://www.youtube.com/watch?v=bzA5aETuIAE)