---
title:                "Rust: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Por que usar expressões regulares em Rust?

Expressões regulares são uma poderosa ferramenta para realizar buscas e manipulações em strings. Em Rust, elas são implementadas através do módulo `regex`, que oferece uma sintaxe amigável e eficiente para trabalhar com padrões de texto.

## Como utilizar expressões regulares em Rust

Para começar a utilizar expressões regulares em Rust, primeiro é necessário adicionar a dependência `regex` ao seu projeto através do gerenciador de pacotes `cargo`. Isso pode ser feito adicionando a seguinte linha ao seu arquivo `Cargo.toml`:
```Rust
[dependencies]
regex = "1.4.2"
```

Com isso, é possível começar a utilizar a sintaxe oferecida pelo módulo `regex`. Por exemplo, para verificar se uma string contém um determinado padrão, pode-se utilizar o método `is_match`, passando o padrão desejado como argumento:
```Rust
use regex::Regex; // Importa o módulo regex

let re = Regex::new(r"hello \w+").unwrap(); // Cria uma expressão regular
println!("Encontrou o padrão? {}", re.is_match("hello world")); // Imprime "true"
```

Também é possível capturar partes específicas de uma string que correspondem a determinados padrões, utilizando as chamadas "capturas" (captures). Por exemplo:
```Rust
let re = Regex::new(r"(\w+), (\w+)").unwrap(); // Com esse padrão, é possível capturar duas palavras separadas por vírgula
let text = "olá, mundo";
let caps = re.captures(text).unwrap(); // Realiza a captura

// Cada captura é acessível através do método "get()"
println!("Capturou as palavras: {} e {}", caps.get(1).unwrap().as_str(), caps.get(2).unwrap().as_str());
// Imprime "Capturou as palavras: olá e mundo"
```

## Mergulhando mais fundo em expressões regulares

Embora o uso básico de expressões regulares em Rust seja bastante intuitivo, existem muitas outras funcionalidades e opções disponíveis no módulo `regex` que permitem realizar tarefas mais avançadas. Alguns desses recursos incluem:

- Busca e substituição de padrões em strings com `replace`
- Iteração sobre todas as correspondências de um padrão em uma string com `find_iter`
- Utilização de classes de caracteres, como `\d` para representar dígitos numéricos, e quantificadores, como `+` para capturar uma ou mais ocorrências de um padrão

Para aprender mais sobre todas as possibilidades oferecidas por expressões regulares em Rust, recomenda-se consultar a documentação oficial do módulo `regex` e praticar com exemplos de uso.

## Veja também

- [Documentação oficial do módulo `regex`](https://docs.rs/regex/1.4.2/regex/)