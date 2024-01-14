---
title:    "Rust: Usando expressões regulares"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares em Rust?

Se você é programador ou está aprendendo a linguagem Rust, provavelmente já ouviu falar sobre expressões regulares. Mas por que elas são úteis? Bem, as expressões regulares são uma maneira eficiente de procurar e manipular padrões de texto em uma string. Elas são muito poderosas e permitem que você faça buscas complexas em texto de forma rápida e precisa.

## Como usar expressões regulares em Rust

Para usar expressões regulares em seu código Rust, você precisará importar a biblioteca padrão `regex`. Em seguida, é preciso compilar a expressão regular que você deseja utilizar e usá-la em conjunto com o método `is_match` para verificar se ela corresponde ao texto desejado. Aqui está um exemplo:

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"([a-z]+) ([0-9]+)").unwrap();
    let text = "Hello 123";
    if re.is_match(text) {
        println!("Correspondência encontrada!");
    } else {
        println!("Nenhuma correspondência encontrada.");
    }
}
```

Neste exemplo, a expressão regular `([a-z]+) ([0-9]+)` procura por qualquer sequência de letras seguida por um espaço e um número. O método `is_match` retorna `true` se houver uma correspondência e `false` se não houver.

Você também pode usar grupos de captura em expressões regulares em Rust. Eles permitem que você extraia partes específicas do texto correspondente. Por exemplo, no código acima, podemos acessar as partes `Hello` e `123` do texto correspondente e usá-las em nosso código.

## Mergulho mais profundo em expressões regulares

Rust possui uma ótima documentação sobre como usar expressões regulares em seu site oficial. Você pode encontrar mais informações sobre os diferentes padrões e métodos que podem ser usados na biblioteca `regex` em https://docs.rs/regex/1.4.6/regex/.

Além disso, se você deseja se aprofundar ainda mais no assunto, existem muitos recursos online que podem ajudá-lo a entender melhor como utilizar as expressões regulares em seu código Rust. Aqui estão alguns links úteis:

- Tutorial sobre expressões regulares em Rust: https://www.tutorialspoint.com/regex/regex_quick_guide.htm
- Documentação detalhada sobre a biblioteca `regex`: https://docs.rs/regex/1.4.6/regex/
- Canal da comunidade Rust no Discord: https://discord.gg/aVES76j

## Veja também

- [Documentação oficial sobre expressões regulares em Rust](https://www.rust-lang.org/pt-BR/documentation.html#regular-expressions)
- [Tutorial sobre expressões regulares em Rust](https://www.tutorialspoint.com/regex/regex_quick_guide.htm)
- [Comunidade Rust no Discord](https://discord.gg/aVES76j)