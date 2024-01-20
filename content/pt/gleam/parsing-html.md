---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-html.md"
---

{{< edit_this_page >}}

# O Que & Porquê?

O parsing de HTML é o processo de leitura e análise de um documento HTML para criar um modelo estruturado. Os programadores fazem isso para extrair dados, modificar conteúdo, ou até para fazer web scraping.

# Como Fazer:

Vamos usar o pacote Gleam `gleam/httpc` para buscar uma página HTTP e o `gleam/decode` para analisar o conteúdo.

```Gleam
import gleam/httpc
import gleam/decode.{field, string}

let response = httpc.get("http://example.com").await
let body = response.body
let tag = field("html", string)

match(decode.body(body, tag)) {
  Error(e) -> io.println(e)
  Ok(tag_value) -> io.println(tag_value)
}
```
Execute o código acima para ver o conteúdo da tag HTML.

# Mergulho Profundo

A análise de HTML chegou a ser um problema desafiador no passado, devido à falta de ferramentas adequadas, mas a situação melhorou muito.

Existem alternativas ao uso do Gleam, tais como Beautiful Soup em Python, ou Cheerio em Node.js. No entanto, Gleam pode ser uma escolha sólida devido ao seu forte sistema de tipos e concorrência eficiente, entre outros benefícios.

Os detalhes de implementação específicos para análise de HTML no Gleam envolvem a utilização de combiners e funções de decodificação. Estes criam uma representação estruturada do conteúdo de HTML, que pode então ser manipulada conforme necessário.

# Veja Também

- Bibliotecas relevantes de Gleam: [https://hex.pm/packages?search=gleam](https://hex.pm/packages?search=gleam) 
- Tutorial de Beautiful Soup para parsing de HTML em Python: [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- Tutorial de Cheerio para parsing de HTML em Node.js: [https://cheerio.js.org/](https://cheerio.js.org/)