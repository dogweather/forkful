---
title:                "Análise de HTML"
html_title:           "Rust: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Existem muitas razões pelas quais alguém pode querer aprender a fazer o parse (analisar) de HTML em Rust. Pode ser para criar um web crawler (rastreador de páginas na web), criar um web scraper (extrator de informações em páginas web) para coletar dados ou até mesmo para criar um site ou aplicativo web personalizado.

## Como fazer

Fazer o parse de HTML em Rust pode ser mais simples do que imagina. Primeiro, precisamos importar a biblioteca "html" em nosso código:

```Rust
use html::{parse_document, parse_fragment, HTMLElement};
```

Em seguida, podemos usar a função parse_document para analisar um documento HTML completo:

```Rust
let document = r#"
    <html>
        <head>
            <title>Rust HTML Parser</title>
        </head>
        <body>
            <h1>Welcome to my website!</h1>
            <div>
                <p>This is an example of parsing HTML in Rust.</p>
            </div>
        </body>
    </html>
"#;
let parsed_document = parse_document(document).unwrap();
```

Também podemos usar a função parse_fragment para analisar apenas uma parte específica de um documento HTML:

```Rust
let fragment = r#"
    <div>
        <p>This is an example of parsing HTML in Rust.</p>
    </div>
"#;
let parsed_fragment = parse_fragment(fragment).unwrap();
```

A partir daqui, podemos usar as funções e métodos da biblioteca para acessar e manipular os elementos HTML, como obter o conteúdo de um elemento específico ou adicionar uma classe a ele.

## Mergulho profundo

A biblioteca "html" em Rust é baseada na especificação HTML DOM (Document Object Model), o que significa que a estrutura e os métodos da biblioteca são bastante similares aos do JavaScript. Isso torna a aprendizagem e uso da biblioteca mais intuitivo para aqueles que já têm experiência em fazer o parse de HTML usando outras linguagens.

Além disso, a biblioteca também suporta parsing de HTML5, o que significa que é possível analisar documentos HTML mais complexos e modernos com facilidade.

## Veja também

- [Documentação da biblioteca html](https://docs.rs/html/)
- [Exemplos de uso da biblioteca html](https://github.com/servo/html5ever/tree/master/html5ever/examples)
- [Tutorial de Rust para iniciantes](https://www.rust-lang.org/pt-BR/learn/get-started)