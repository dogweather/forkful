---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-html.md"
---

{{< edit_this_page >}}

# Entendendo Parsing de HTML com Rust

## O Que & Por Quê?

Parsing de HTML é o processo de converter HTML bruto em uma estrutura de dados que podemos manipular em nossos programas. Fazemos isso para extrair informações, manipular a estrutura do HTML, ou até mesmo para realizar verificações de consistência.

## Como fazer:

Rust nos fornece várias bibliotecas para parsing de HTML. Vamos usar a biblioteca `scraper` para um exemplo simples.

```rust
// Adiciona a dependência ao seu arquivo Cargo.toml
[dependencies]
scraper = "0.3.0"

// Utilização básica: 

use scraper::{Html, Selector};

fn main() {
    let html = Html::parse_document("<html><body><p>Hello world!<p></body></html>");
    let selector = Selector::parse("body").unwrap();

    let body = html.select(&selector).next().unwrap();
    assert_eq!(body.text().collect::<Vec<_>>(), vec!["Hello world!"]);
}
```

Neste exemplo, estamos fazendo parse de um snippet HTML simples para selecionar o elemento `body` e extrair o texto correspondente.

## Mergulho Profundo

O parsing de HTML existe desde o início da web, sendo inicialmente uma tarefa de scripts Perl ou JavaScript do lado do servidor. Com Rust, obtemos um desempenho muito melhor e segurança na memória.

Existem alternativas ao 'scraper', como 'html5ever' ou 'kuchiki'. Cada um vem com suas próprias vantagens, sendo o 'html5ever' muito rápido, mas menos amigável com relação ao usuário se comparado ao 'scraper'.

Os detalhes da implementação dependem muito do que você está tentando alcançar. Se você estiver fazendo scraping em um site inteiro, por exemplo, você vai querer usar um cliente HTTP robusto, tratar links etc. Outras considerações são relativas ao desempenho - se você estiver lidando com grandes quantidades de HTML, você vai querer ter cuidado com o uso de memória.

## Veja Também

Os seguintes recursos podem ser úteis para aprofundar seu conhecimento em Parsing de HTML com Rust:

1. Documentação oficial do Rust: [Rust HTML Parsing](https://docs.rs/html5ever/0.25.1/html5ever/)

2. Postagem de blog detalhada: [Parsing HTML with Rust](https://www.programming-idioms.org/idiom/68/parse-html/2316/rust)

3. Stack Overflow para dúvidas relacionadas: [Stack Overflow 'Rust' + 'HTML Parsing'](https://stackoverflow.com/questions/tagged/rust+html-parsing)