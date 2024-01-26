---
title:                "Análise de HTML"
date:                  2024-01-20T15:31:55.128471-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Analisar HTML é o processo de interpretar o código desse tipo de documento para extrair informações específicas – um trabalho comum quando se quer automatizar a manipulação de dados de websites. Programadores fazem isso para processar conteúdos da web, seja para coletar dados (web scraping), testar funcionalidades de páginas ou construir ferramentas como leitores de RSS.

## Como Fazer:
Gleam ainda está crescendo no contexto de análise de HTML. Vamos usar uma biblioteca hipotética `gleam_html_parser` para ilustrar. Não se esqueça de adicionar a biblioteca ao seu `gleam.toml`.

```gleam
import gleam_html_parser

fn main() {
  let html_content = "<!DOCTYPE html><html><head><title>Exemplo</title></head><body><h1>Oi, Gleam!</h1></body></html>"

  let parsed_data = gleam_html_parser.parse(html_content)
  
  assert Ok(document) = parsed_data
  let headlines = gleam_html_parser.find_all(document, "h1")
  assert Ok(elements) = headlines
  let text_list = elements
    |> list.map(fn(element) {
      gleam_html_parser.get_text(element)
  })
  assert Ok(["Oi, Gleam!"]) = text_list

  io.println(text_list)
}
```
Saída esperada:
```
["Oi, Gleam!"]
```

## Mergulhando Fundo:
Historicamente, Gleam surge como uma linguagem estática e segura para a Erlang VM. A análise de HTML nela e em outras linguagens tende a envolver bibliotecas como Beautiful Soup (Python) ou Nokogiri (Ruby). Em Gleam, pode-se esperar uma evolução similar com a criação de bibliotecas especializadas. Um ponto-chave é manipular a análise de forma a lidar com HTML malformado - algo comum quando raspando a web. Alternativas incluem usar regulares, o que geralmente é desaconselhado, ou ferramentas como `lex` e `yacc` que são mais robustas mas têm uma curva de aprendizado mais íngreme.

## Veja Também:
- Documentação oficial do Gleam: [https://gleam.run/](https://gleam.run/)
- Comunidade Gleam no GitHub para discutir sobre bibliotecas e ferramentas: [https://github.com/gleam-lang/gleam](https://github.com/gleam-lang/gleam)
- Tutorial de Web Scraping com outras linguagens: [https://realpython.com/beautiful-soup-web-scraper-python/](https://realpython.com/beautiful-soup-web-scraper-python/)
