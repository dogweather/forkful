---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# Embainhar com Haskell: Como Fazer Parse de HTML

## O quê e Por quê?

Parse de HTML é a extração de dados estruturados a partir de código HTML. Programadores fazem isso para manipular, analisar ou extrair informações específicas de páginas da web.

## Como Fazer:

A biblioteca `tagsoup` é uma excelente opção em Haskell para fazer parse de HTML. Aqui está um exemplo rápido:

```Haskell
import Text.HTML.TagSoup

htmlParse :: String -> IO ()
htmlParse url = do
  -- pegar o HTML da URL
  html <- openURL url

  -- fazer o parse das tags HTML e exibir os textos das tags que são links
  let tags = parseTags html
  putStrLn "Links na página:"
  mapM_ print [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- função auxiliar para pegar o código HTML de uma URL
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)
```

Isso listará todos os links de uma página da web especificada. Claro, esse é um exemplo simples para ilustrar como fazer o parse de HTML.

## Mergulho Profundo

A análise de HTML tem uma história longa desde a criação da web, e Haskell tem várias bibliotecas para lidar com ela, cada uma com seus próprios prós e contras.

Alternativas ao `tagsoup` incluem `html-conduit` e `pandoc`. A escolha depende das suas necessidades: 
- `html-conduit` é útil se a entrada for grande ou se for potencialmente infinita, como uma transmissão ao vivo.
- `pandoc` é perfeito para converter HTML em outros formatos de markup.

A biblioteca `tagsoup` em si faz uma análise tolerante a falhas, o que é bom para lidar com HTML real na web, que pode não estar bem formatado.

## Veja Também

- A documentação do `tagsoup`: https://hackage.haskell.org/package/tagsoup
- Como parsear HTML com Conduit: https://www.yesodweb.com/book/xml
- Converter HTML com o Pandoc: https://pandoc.org/MANUAL.html#readers