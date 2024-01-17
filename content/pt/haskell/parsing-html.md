---
title:                "Analisando o HTML"
html_title:           "Haskell: Analisando o HTML"
simple_title:         "Analisando o HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/parsing-html.md"
---

{{< edit_this_page >}}

# O que e Por que?

Que tal já ter ouvido falar daquela linda página na internet que você queria extrair informações? Ou então, você queria automatizar a coleta desses dados para usá-los em um projeto de análise ou visualização? É exatamente para isso que serve o parsing HTML!

Parsing HTML é basicamente o processo de analisar o código fonte de uma página HTML para extrair informações específicas, como links, textos e imagens. Programadores muitas vezes fazem isso para automatizar tarefas e obter dados úteis de uma forma mais organizada.

# Como Fazer:

Para realizar o parsing HTML em Haskell, podemos utilizar a biblioteca bem conhecida chamada "tagsoup". Com ela, podemos facilmente carregar uma página HTML e selecionar elementos específicos usando sintaxe simples.

```
import Text.HTML.TagSoup

paginaHtml <- openURL "https://www.example.com/"

let links = filter (isTagOpenName "a") $ parseTags paginaHtml

print links
```

No código acima, carregamos uma página da internet e usamos a função "parseTags" da biblioteca para converter o código HTML em uma lista de tags. Em seguida, usamos a função "filter" para selecionar apenas as tags do tipo "a" (links) e, finalmente, imprimimos os links obtidos.

# Mergulho Profundo:

O parsing HTML é uma técnica antiga e amplamente utilizada para extrair informações de páginas da web. Existem várias outras bibliotecas disponíveis, como "html-parser" e "html-conduit", que podem ser usadas para o mesmo propósito.

Na maioria dos casos, é necessário realizar algum pré-processamento no código HTML antes de começar a extrair informações, como remover comentários ou tags desnecessárias. Além disso, é importante estar ciente das possíveis limitações e inconsistências no código HTML que podem dificultar o processo de parsing.

# Veja Também:

- Documentação da biblioteca tagsoup: https://hackage.haskell.org/package/tagsoup
- Outras bibliotecas para parsing HTML em Haskell: https://wiki.haskell.org/Handling_HTML