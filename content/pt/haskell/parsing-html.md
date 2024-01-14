---
title:                "Haskell: Mapeando html"
simple_title:         "Mapeando html"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Por que fazer parsing HTML em Haskell?

Fazer parsing de HTML pode ser uma tarefa complexa em qualquer linguagem de programação, mas com a ajuda do Haskell, pode se tornar uma tarefa muito mais eficiente e efetiva. Ao utilizar o poderoso sistema de tipos e a sintaxe funcional clara do Haskell, é possível criar um parser de HTML robusto e confiável em pouco tempo.

## Como Fazer
```Haskell
-- Importando os módulos necessários
import Text.HTML.TagSoup -- Utilizado para fazer o parsing do HTML
import Network.HTTP.Simple -- Utilizado para fazer requisições HTTP

-- Função para obter um documento HTML a partir de uma URL
getHTML :: String -> IO String
getHTML url = do
    request <- parseRequest url
    response <- httpLBS request
    let responseBody = getResponseBody response
    return . show $ responseBody

-- Função para fazer parsing do HTML e retornar uma lista de tags
parseHTML :: String -> [Tag String]
parseHTML html = parseTags html
```

A função `getHTML` utiliza o módulo `Network.HTTP.Simple` para fazer uma requisição HTTP e obter um documento HTML a partir de uma URL. Em seguida, a função `parseHTML` utiliza o módulo `Text.HTML.TagSoup` para fazer o parsing do HTML e retornar uma lista de tags. Com estas duas funções, é possível obter e parsear um documento HTML facilmente.

```Haskell
-- Exemplo de utilização
main :: IO ()
main = do
    html <- getHTML "https://www.example.com"
    let tags = parseHTML html
    print tags
```

Saída:
```
[TagOpen "html" [],TagOpen "head" [],TagOpen "title" [],TagText "Example Domain",TagClose "title",TagOpen "meta" [TagClose "meta",TagOpen "body" [],TagClose "body",TagClose "html"]
```

## Dive Profundo
Para aqueles que desejam se aprofundar no assunto, o Haskell oferece inúmeras ferramentas e recursos que podem ser utilizados para criar um parser de HTML ainda mais avançado. Alguns módulos que podem ser úteis incluem:

- `Text.HTML.TagSoup.Tree`: Este módulo expande as funcionalidades do `Text.HTML.TagSoup` ao oferecer uma estrutura de dados de árvore que pode ser utilizada para navegar pelo documento HTML de forma mais organizada.
- `Data.Text`: Este módulo oferece diversas funções úteis para trabalhar com texto e pode ser utilizado para manipular as tags e conteúdo do documento HTML.
- `Data.Char`: Este módulo oferece funções para trabalhar com caracteres, o que pode ser útil para filtrar e validar as tags ou conteúdo do documento HTML.

Com a combinação dos módulos certos e um conhecimento sólido sobre Haskell, é possível criar um parser de HTML eficiente e personalizado.

## Veja Também
- [Documentação do módulo Text.HTML.TagSoup](https://hackage.haskell.org/package/tagsoup/docs/Text-HTML-TagSoup.html)
- [Documentação do módulo Network.HTTP.Simple](https://hackage.haskell.org/package/http-client/docs/Network-HTTP-Simple.html)
- [Tutorial de Haskell para Iniciantes](https://haskellfromthebeginning.com/)
- [Exemplos de parsers de HTML em Haskell](https://github.com/search?q=Haskell+HTML+parser)