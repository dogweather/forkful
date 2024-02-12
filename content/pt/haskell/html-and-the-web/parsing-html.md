---
title:                "Analisando HTML"
aliases:
- /pt/haskell/parsing-html.md
date:                  2024-02-03T19:12:11.815629-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Porquê?

Analisar HTML em Haskell permite extrair dados, manipular conteúdo HTML ou interagir com páginas web programaticamente. Essa operação é essencial para tarefas como raspagem de web, testes automatizados de aplicações web e mineração de dados a partir de websites - aproveitando o sistema de tipos forte de Haskell e os paradigmas de programação funcional para garantir um código robusto e conciso.

## Como Fazer:

Para analisar HTML em Haskell, utilizaremos a biblioteca `tagsoup` pela sua simplicidade e flexibilidade. Primeiro, certifique-se de instalar a biblioteca adicionando `tagsoup` ao arquivo cabal do seu projeto ou executando `cabal install tagsoup`.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- HTML de amostra para demonstração
let sampleHtml = "<html><body><p>Aprenda Haskell!</p><a href='http://example.com'>Clique Aqui</a></body></html>"

-- Analisa o HTML e filtra por links (tags a)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- Imprime os links extraídos
print links
```

Saída de amostra:
```plaintext
["http://example.com"]
```

Para necessidades mais sofisticadas de análise de HTML, considere usar a biblioteca `pandoc`, especialmente se estiver trabalhando com conversão de documentos. É excepcionalmente versátil, mas vem com mais complexidade:

```haskell
import Text.Pandoc

-- Assumindo que você tem um documento Pandoc (doc) carregado, por exemplo, de ler um arquivo
let doc = ... -- Seu documento Pandoc vai aqui

-- Converte o documento para uma string HTML
let htmlString = writeHtmlString def doc

-- Agora, você analisaria `htmlString` como acima ou prosseguiria conforme suas necessidades.
```
Lembre-se de que `pandoc` é uma biblioteca muito maior, focada na conversão entre inúmeros formatos de marcação, então use-a se precisar dessas capacidades extras ou se já estiver lidando com formatos de documentos em sua aplicação.
