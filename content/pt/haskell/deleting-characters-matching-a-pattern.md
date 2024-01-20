---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Quê & Por quê?

Eliminar caracteres que combinam com um padrão é uma tarefa comum em programação. Permite-nos manipular e limpar dados, e é frequentemente usado em tarefas como a extração de dados importantes de grandes blocos de texto.

## Como fazer:

No Haskell, você pode usar Data.Text para manipular strings. Aqui está um exemplo códigos de como deletar caracteres que correspondem a um padrão:

```Haskell
import Data.Text as T

-- remover espaços de uma string
removerEspaco :: Text -> Text
removerEspaco = T.filter (/= ' ')

```
Exemplo de uso:

```Haskell
removerEspaco "Haskell é incrível"
```

Que resultará na saída: `"Haskelléincrível"`

Note que a função `filter` foi utilizada. Ela percorre cada caractere e remove os que atendem ao critério especificado (' ' neste caso).

## Aprofundando

Eliminar caracteres é algo que os programadores têm feito há décadas. A necessidade surgiu desde os primeiros dias da programação, quando o armazenamento de dados era limitado e era crucial remover informações desnecessárias.

No Haskell, a abordagem padrão para tal tarefa é usando "Data.Text". No entanto, se você estiver lidando com arquivos enormes, pode considerar outras bibliotecas Haskell, como "Data.Text.Lazy" ou "Data.ByteString.Lazy" para melhor desempenho.

Internamente, a função filter é implementada como uma recursão de cauda, que é uma técnica comum em Haskell e outras linguagens funcionais. Ela examina cada caractere da string um por um, decidindo se deve manter ou excluir com base no critério fornecido.

## Veja Também

Links úteis para aprofundar neste tópico:

[Haskell: Data.Text Documentation](https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html)

[Haskell: Data.Text.Lazy Documentation](https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text-Lazy.html)

[Haskell: Data.ByteString.Lazy Documentation](https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString-Lazy.html)

[Stack Overflow: filter function in Haskell](https://stackoverflow.com/questions/2851683)