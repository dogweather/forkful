---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## O Que São & Porquê Usar?
Expressões regulares são padrões usados para encontrar correspondências de texto. Programadores as usam para busca e substituição de texto, validação de entrada de dados e análise de texto complexo de forma eficiente.

## Como Fazer:
```Rust
extern crate regex;
use regex::Regex;

fn main() {
    let re = Regex::new(r"(\w+)@(\w+)\.com").unwrap();
    let email = "hello@world.com";

    // Verifica se a string corresponde à expressão regular
    println!("Email válido? {}", re.is_match(email));  // Saída: Email válido? true

    // Encontra e imprime as partes capturadas pelo grupo
    if let Some(captures) = re.captures(email) {
        println!("Usuário: {}", captures.get(1).unwrap().as_str());  // Saída: Usuário: hello
        println!("Domínio: {}", captures.get(2).unwrap().as_str()); // Saída: Domínio: world
    }
}
```

## Mergulho Profundo
1. **Contexto Histórico**: As expressões regulares têm origem nos anos 50 com trabalhos sobre a teoria dos autômatos. Sua inclusão em linguagens de programação adquiriu popularidade com o Perl.
2. **Alternativas**: Há outras formas de processar texto, como o uso de parsers específicos para línguas naturais ou sistemas baseados em IA. Contudo, para validações simples, as expressões regulares são mais leves e rápidas.
3. **Detalhes de Implementação**: Rust usa a biblioteca `regex`, que compila as expressões regulares para um autômato que realiza a busca. Isso é diferente de outras linguagens que podem usar interpretação direta, o que pode ser mais lento.

## Veja Também
- Documentação do `regex` crate: [https://docs.rs/regex](https://docs.rs/regex)
- Tutorial "The Rust Programming Language" para manipulação de strings: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
