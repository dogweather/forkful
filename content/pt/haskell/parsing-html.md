---
title:                "Análise de HTML"
date:                  2024-01-20T15:32:07.801546-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Analisar HTML significa extrair dados estruturados de documentos HTML. Programadores fazem isso para automatizar a recuperação de informações de páginas web, como para web scraping ou para alimentar dados em aplicativos.

## Como Fazer:
Experimente a biblioteca `tagsoup`, uma maneira fácil de mexer com HTML em Haskell. Não é estritamente um parser XML/HTML pois é tolerante a erros, sendo perfeita para lidar com o HTML "real" que encontramos.

```haskell
import Text.HTML.TagSoup

-- Exemplo: Encontrar todos os hyperlinks em um documento HTML
extractLinks :: String -> [String]
extractLinks html = [href | TagOpen "a" attrs <- parseTags html, ("href", href) <- attrs]

-- Use a função acima em algum HTML de exemplo
main :: IO ()
main = do
    let htmlSample = "<html><body><a href='http://example.com'>Example</a></body></html>"
    print $ extractLinks htmlSample
```

Resultado esperado será simplesmente:

```
["http://example.com"]
```

## Mergulho Profundo:
Haskell tem uma reputação por ser ótimo com tarefas de parsing, graças à sua precisão e expressividade. A `tagsoup` foi desenvolvida para oferecer uma forma robusta e indulgente de fazer parsing de HTML, ideal para a natureza imprevisível do HTML na internet. Alternativas como `xeno` e `html-conduit` existem e podem ser mais adequadas se precisar de conformidade estrita com os padrões XML/HTML.

Ao trabalhar com parsing de HTML, deve-se considerar também asdiretrizes éticas e legais; scraping de sites sem permissão pode ser controverso ou até ilegal em alguns contextos.

## Veja Também:
- Documentação da `tagsoup`: https://hackage.haskell.org/package/tagsoup
- Biblioteca `xeno`: https://hackage.haskell.org/package/xeno
- Biblioteca `html-conduit` para parsing mais rigoroso: https://hackage.haskell.org/package/html-conduit
- Um bom tutorial sobre web scraping com Haskell: https://www.schoolofhaskell.com/school/advanced-haskell/beautiful-concurrency/3-web-scraping-part-1
