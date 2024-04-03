---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:24.229385-07:00
description: "Como fazer: A biblioteca `regex` de Rust \xE9 a indicada para trabalhar\
  \ com express\xF5es regulares. Para us\xE1-la, voc\xEA primeiro precisa adicion\xE1\
  -la ao seu\u2026"
lastmod: '2024-03-13T22:44:46.357969-06:00'
model: gpt-4-0125-preview
summary: "A biblioteca `regex` de Rust \xE9 a indicada para trabalhar com express\xF5\
  es regulares."
title: "Usando express\xF5es regulares"
weight: 11
---

## Como fazer:
A biblioteca `regex` de Rust é a indicada para trabalhar com expressões regulares. Para usá-la, você primeiro precisa adicioná-la ao seu `Cargo.toml`:

```toml
[dependencies]
regex = "1"
```

Então, você pode começar a implementar funcionalidades de regex no seu código Rust. Aqui está como realizar algumas operações comuns:

### Correspondendo um Padrão em uma String
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("O texto corresponde ao padrão de data? {}", re.is_match(date));
    // Saída: O texto corresponde ao padrão de data? true
}
```

### Encontrando e Acessando Correspondências
```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Linguagem: {}, Ano: {}", &cap[1], &cap[2]);
    }
    // Saída:
    // Linguagem: Rust, Ano: 2023
    // Linguagem: C++, Ano: 2022
    // Linguagem: Python, Ano: 2021
}
```

### Substituindo Texto
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 foi atualizado em $2");

    println!("Texto atualizado: {}", replaced);
    // Saída: Texto atualizado: Rust foi atualizado em 2023, C++ foi atualizado em 2022, Python foi atualizado em 2021
}
```

### Dividindo Texto Usando um Regex
```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // divide em qualquer caractere não-palavra
    let text = "Rust-C++-Python-Go";

    let campos: Vec<&str> = re.split(text).collect();

    for campo in campos {
        println!("Linguagem: {}", campo);
    }
    // Saída:
    // Linguagem: Rust
    // Linguagem: C++
    // Linguagem: Python
    // Linguagem: Go
}
```

Estes exemplos oferecem um guia básico para começar com expressões regulares em Rust. Conforme suas necessidades se tornam mais sofisticadas, o crate `regex` oferece uma grande quantidade de funcionalidades para tarefas complexas de correspondência de padrões e manipulação de texto.
