---
title:                "Analisando uma data a partir de uma string"
aliases: - /pt/rust/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:25.143802-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando uma data a partir de uma string"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar uma data a partir de uma string é uma tarefa comum ao lidar com entrada de usuário ou leitura de dados de arquivos, o que envolve converter os dados em string para um formato de data reconhecido pela linguagem de programação. Em Rust, isso é essencial para operações com datas, como comparações, aritmética ou formatação, e melhora a validação e integridade dos dados em aplicações.

## Como Fazer:

### Usando a Biblioteca Padrão do Rust (`chrono` Crate)
A biblioteca padrão do Rust não inclui diretamente a análise de datas, mas o `crate chrono`, amplamente utilizado, é uma solução robusta para manipulação de data e hora. Primeiro, adicione `chrono` ao seu `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Então, use `chrono` para analisar uma string de data em um objeto `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Falha ao analisar data");

    println!("Data analisada: {}", date);
}

// Saída de Exemplo:
// Data analisada: 2023-04-01
```

### Usando o Manejo Avançado de Data e Hora do Rust (`time` Crate)
Para um manuseio mais avançado de data e hora, incluindo análise mais ergonômica, considere o `crate time`. Primeiro, inclua-o no seu `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Então, analise uma string de data usando o tipo `Date` e `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Falha ao analisar data e hora");

    println!("Data e hora analisadas: {}", parsed_date);
}

// Saída de Exemplo:
// Data e hora analisadas: 2023-04-01 12:34:56
```

Ambos os exemplos demonstram como o Rust, com a ajuda de crates de terceiros, facilita a análise de strings de data em objetos de data manipuláveis, tornando-se uma ferramenta poderosa para o desenvolvimento de software que envolve dados temporais.
