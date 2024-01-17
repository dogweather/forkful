---
title:                "Analisando HTML"
html_title:           "Rust: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/parsing-html.md"
---

{{< edit_this_page >}}

## O quê e por quê?

Parsing HTML é o processo de analisar um documento HTML e extrair informações específicas dele. Programadores utilizam o parsing HTML para automatizar tarefas, como por exemplo, extrair dados de uma página web para serem usados em outro lugar.

## Como fazer:
```Rust
use scraper::{Html, Selector};
 
fn main() {
    // Cria uma variável com o documento HTML para ser analisado
    let html = r#"<!DOCTYPE html>
    <html>
        <head>
            <title>Exemplo de página</title>
        </head>
        <body>
            <p>Este é um parágrafo de exemplo.</p>
            <ul>
                <li>Item 1</li>
                <li>Item 2</li>
                <li>Item 3</li>
            </ul>
        </body>
    </html>"#;
    
    // Cria um seletor para os elementos <li>
    let selector = Selector::parse("li").unwrap();
    
    // Converte o documento HTML em uma estrutura de árvore
    let document = Html::parse_document(html);
    
    // Utiliza o seletor para buscar os elementos <li>
    for li in document.select(&selector) {
        // Imprime o conteúdo de cada elemento <li>
        println!("{}", li.inner_html());
    }
}
```
Output:
```
Item 1
Item 2
Item 3
```

## Aprofundando:
O parsing HTML é um processo fundamental para a web, pois permite que páginas sejam lidas e interpretadas por computadores. Existem diferentes formas de realizar o parsing HTML, como a utilização de bibliotecas ou ferramentas específicas para essa tarefa.

## Veja também:
- [Documentação oficial do Rust sobre parsing HTML](https://docs.rs/scraper/0.9.0/scraper/)
- [Exemplo prático de parsing HTML com Rust](https://www.taniarascia.com/web-scraping-with-rust/)
- [Outra biblioteca de parsing HTML para Rust](https://github.com/selects/select.rs)
- [Ferramenta para testar seletores e realizar parsing HTML online](https://try.jsoup.org/)