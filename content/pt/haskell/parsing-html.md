---
title:                "Analisando HTML"
html_title:           "Haskell: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de extrair informações de uma página da web? Ou talvez queira automatizar tarefas que envolvem conteúdo HTML? Nesse caso, a habilidade de fazer parse de HTML usando Haskell pode ser útil.

## Como fazer

Fazer parse de HTML em Haskell pode parecer complicado, mas com algumas ferramentas e conhecimento básico de programação funcional, é possível realizar essa tarefa de forma eficiente. Abaixo, segue um exemplo de como fazer parse de uma página simples usando a biblioteca [tagsoup](https://hackage.haskell.org/package/tagsoup):

```Haskell
import Text.HTML.TagSoup

main = do
  let site = "<html><head><title>Exemplo</title></head><body><h1>Título</h1><p>Conteúdo</p></body></html>"
  let tags = parseTags site
  let title = fromTagText $ head $ dropWhile (~/= "<title>") tags
  let content = fromTagText $ head $ dropWhile (~/= "<p>") tags
  putStrLn $ "Título: " ++ title
  putStrLn $ "Conteúdo: " ++ content
```

A saída desse código será:

```
Título: Exemplo
Conteúdo: Conteúdo
```

Nesse exemplo, usamos a função `parseTags` para transformar uma string de HTML em uma lista de tags. Em seguida, utilizamos a função `fromTagText` para obter o texto dentro das tags específicas que queremos. Por fim, imprimimos esses textos na tela usando a função `putStrLn`.

## Mergulho Profundo

Para entender melhor como funciona o processo de parsing de HTML em Haskell, é importante conhecer alguns conceitos básicos de programação funcional. Em Haskell, uma forma de representar dados é através de tipos algébricos, que permitem estruturar dados de forma hierárquica e recursiva.

No caso de HTML, podemos usar tipos algébricos para representar as diferentes tags e seus atributos. Por exemplo, podemos definir um tipo `Tag` que pode ser `Text` (representando texto) ou `Elem` (representando uma tag contendo um nome e uma lista de atributos). Dessa forma, podemos percorrer as diferentes tags do HTML e extrair informações específicas de acordo com sua estrutura.

Outra ferramenta útil para fazer parse de HTML é a biblioteca [attoparsec](https://hackage.haskell.org/package/attoparsec), que possui uma interface de alto nível para analisar strings de texto. Com essa biblioteca, é possível criar parsers para diferentes tipos de dados, incluindo HTML.

## Veja também

- [Haskell para iniciantes](https://haskell.org/tutorial)
- [Documentação da biblioteca tagsoup](https://hackage.haskell.org/package/tagsoup)
- [Documentação da biblioteca attoparsec](https://hackage.haskell.org/package/attoparsec)