---
title:    "Rust: Calculando uma data no futuro ou passado"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que

Se você é um programador em busca de uma linguagem moderna e eficiente para desenvolver suas aplicações, então pode ser que você já tenha ouvido falar sobre Rust. Essa linguagem de programação está ganhando cada vez mais popularidade devido ao seu desempenho e segurança. E uma das funcionalidades interessantes do Rust é a possibilidade de calcular datas no futuro ou passado.

## Como fazer

Para calcular uma data no futuro ou passado em Rust, podemos usar a biblioteca `chrono`. Essa biblioteca oferece diversas funções e estruturas para trabalhar com datas e horas de forma simples e eficiente. Vamos ver um exemplo de como calcular a data de amanhã utilizando `chrono`:

```
use chrono::{Utc, Duration};

fn main() {
    let today = Utc::today(); // obtém a data atual no timezone UTC
    let one_day = Duration::days(1); // cria um intervalo de 1 dia
    let tomorrow = today + one_day; // soma o intervalo à data atual
    println!("{}", tomorrow); // imprime a data de amanhã no formato padrão
}
```

Ao executar esse código, teremos o seguinte resultado:

`2021-01-17`

## Aprofundando-se

Além de calcular datas no futuro ou passado, a biblioteca `chrono` também permite realizar outras operações interessantes, como extrair informações específicas de uma data, converter para diferentes formatos ou timezones, entre outras. Vale a pena explorar a documentação oficial para descobrir mais funcionalidades dessa biblioteca.

Outro ponto importante é lembrar que datas e horas podem ser um tanto complexas, com diferentes calendários, fusos horários e noções de tempo. Por isso, o uso de uma biblioteca como `chrono` pode facilitar muito o trabalho de lidar com esses conceitos.

## Veja também

- Documentação oficial da biblioteca `chrono`: https://docs.rs/chrono/
- Exemplos de cálculo de datas com `chrono`: https://www.educative.io/blog/tutorial-rust-date-strings
- Rust Brasil - Comunidade brasileira de Rust: https://rust-br.org/