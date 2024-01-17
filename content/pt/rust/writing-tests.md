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

## O que e por que?

Escrever testes em programação é um processo de criar pequenos programas que verificam a funcionalidade do seu código principal. Os programadores fazem isso para garantir que seu código esteja funcionando corretamente e para evitar erros futuros em seu desenvolvimento.

## Como fazer:

O Rust possui sua própria biblioteca de testes integrada, permitindo a escrita de testes diretamente no código fonte. Para criar um novo teste, basta adicionar o atributo `test` acima da função do teste e executá-lo usando o comando `cargo test`.

```Rust
#[test]
fn test_soma() {
    assert_eq!(2 + 2, 4);
}
```

Se todos os testes passarem com sucesso, você verá uma mensagem indicando que todos os testes foram aprovados.

```
Compiling projeto v0.1.0 (file:///C:/projetos/rust/projeto)
Finished in 0.063s
Running target\debug\projeto-9bb9194203a976bb.exe

running 1 test
test test_soma ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured

   Running `target\debug\projeto-9bb9194203a976bb.exe`
```

## Profundidade:

Os testes em programação têm sido cada vez mais importantes à medida que os projetos se tornam maiores e mais complexos. Eles também são uma prática recomendada em metodologias ágeis de desenvolvimento, pois ajudam a detectar problemas no início e a manter o código mais limpo e organizado.

Além da biblioteca de testes do Rust, existem outras alternativas, como a biblioteca de testes `assert`, que oferece funcionalidades adicionais e personalização. É importante escolher a biblioteca de testes mais adequada para o seu projeto, levando em consideração suas necessidades e preferências.

No que diz respeito à implementação, os testes são compilados em uma seção separada do executável final e podem ser facilmente ignorados se você não quiser executá-los. Além disso, o Rust possui uma sintaxe clara e concisa para a criação de testes, tornando o processo mais fácil e rápido.

## Veja também:

Para saber mais sobre testes em Rust, confira a documentação oficial em https://doc.rust-lang.org/book/testing.html e o repositório de exemplos da comunidade em https://github.com/rust-unofficial/awesome-rust#testing.