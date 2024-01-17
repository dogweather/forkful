---
title:                "Buscando e substituindo texto"
html_title:           "Gleam: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Substituir e buscar textos é uma tarefa comum entre os programadores. Isso envolve localizar determinado texto em um arquivo ou documento e substituí-lo por outro. Fazemos isso para economizar tempo e reduzir erros manuais.

# Como fazer:

Usando o Gleam, podemos facilmente substituir e buscar textos em arquivos. Primeiro, importamos o módulo "Text" e, em seguida, usamos as funções "replace" e "find" para localizar e substituir o texto desejado. Aqui está um exemplo de como fazer isso:

```Gleam

import Text

let nome = "João"

nome = Text.replace(nome, "João", "Pedro")
// Output: Pedro
Text.find("Pedro", "Isso é uma string contendo o nome Pedro.")
// Output: true

```

# Mergulho profundo:

Substituir e buscar textos é uma tarefa comum na programação, especialmente quando estamos trabalhando com grandes quantidades de código ou lidando com múltiplos arquivos. Antes do surgimento de linguagens de programação e ferramentas de edição, essa tarefa era realizada manualmente, o que era demorado e propenso a erros. Com o avanço da tecnologia, surgiram ferramentas especiais para facilitar essa tarefa, como o Gleam.

Existem diversas alternativas ao Gleam para substituir e buscar textos, como o Bash, Perl e Python. No entanto, o Gleam se destaca por sua simplicidade e facilidade de uso, além de ser uma linguagem de programação funcional pura. Além disso, o Gleam é altamente compatível com outras ferramentas e linguagens, o que facilita sua integração em projetos maiores.

Ao procurar e substituir textos com o Gleam, é importante ter em mente que ele é sensível a maiúsculas e minúsculas. Isso significa que se você estiver procurando por "João", ele não encontrará "joão". Além disso, o Gleam também pode trabalhar com expressões regulares, o que permite maior flexibilidade na busca por padrões específicos de texto.

# Veja também:

Se você deseja aprender mais sobre como o Gleam pode ser usado para substituir e buscar textos, aqui estão alguns recursos úteis:

- Documentação oficial do Gleam
- Exemplos de código para buscar e substituir textos usando o Gleam
- Um tutorial em vídeo sobre a utilização do Gleam para buscar e substituir textos em nossos projetos.