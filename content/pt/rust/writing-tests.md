---
title:                "Escrevendo testes"
html_title:           "Rust: Escrevendo testes"
simple_title:         "Escrevendo testes"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-tests.md"
---

{{< edit_this_page >}}

## Por que escrever testes é importante?

Escrever testes pode parecer uma tarefa tediosa e desnecessária para alguns, mas na verdade é uma parte crucial no processo de desenvolvimento de software. Além de garantir que o código funcione corretamente, os testes também ajudam a identificar e corrigir possíveis bugs antes que eles cheguem aos usuários finais.

## Como escrever testes em Rust

Escrever testes em Rust é uma tarefa simples e direta. Primeiro, é importante criar uma função de teste marcada com o atributo `#[test]`, seguida por uma chamada à função `assert!` que verifica se o resultado esperado é igual ao resultado real. Aqui está um exemplo de uma função de teste simples que verifica se dois números são iguais:

```Rust
#[test]
fn test_equality() {
    assert!(10 == 5 + 5);
}
```

Ao rodar o comando `cargo test`, você verá uma saída indicando se o teste foi aprovado ou falhou. Se você quiser testar uma função com argumentos, basta passá-los dentro dos parênteses da função `assert!`.

## Aprofundando nos testes em Rust

Existem várias ferramentas disponíveis em Rust para escrever testes mais avançados, como o módulo `std::assert` que oferece mais opções de checagem de valores. Também é possível criar testes que verifiquem se uma função específica retorna um erro esperado. Para isso, basta utilizar o atributo `#[should_panic]` na função de teste. Além disso, é possível criar testes estruturados em diferentes módulos e executá-los em paralelo para aumentar a eficiência dos testes.

## Veja também
- [Documentação oficial do Rust sobre testes](https://doc.rust-lang.org/book/testing.html)
- [Exemplos práticos de testes em Rust](https://www.rust-lang.org/learn/get-started)
- [Artigo sobre a importância dos testes em desenvolvimento de software](https://medium.com/@chrisng93/the-importance-of-testing-in-software-development-290589c1c2d6)