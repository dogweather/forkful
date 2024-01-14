---
title:    "Haskell: Buscando e substituindo texto"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

A tarefa de buscar e substituir texto é uma das tarefas mais comuns e úteis na programação, especialmente em linguagens funcionais como Haskell. Ao dominar essa habilidade, você poderá economizar tempo e aumentar sua produtividade na escrita de código.

## Como fazer

Existem várias maneiras de buscar e substituir texto em Haskell, mas uma das maneiras mais simples é usando a função `replaceAll` da biblioteca `Data.Text`, que recebe uma lista de pares de texto: o texto a ser substituído e o texto substituto.

```
Haskell
import Data.Text (unpack, replaceAll)
import qualified Data.Text as T

-- uma string de exemplo
texto = "Olá, mundo!"

-- substituindo "mundo" por "Haskell"
novoTexto = replaceAll (T.pack "mundo") (T.pack "Haskell") (T.pack texto)

unpack novoTexto -- imprime "Olá, Haskell!"
```

Você também pode usar expressões regulares para realizar buscas e substituições em texto. A biblioteca `Data.Text.Regex` possui funções úteis para lidar com expressões regulares em Haskell. Aqui está um exemplo de como usar a função `subRegex` para substituir todas as ocorrências de números por asteriscos em uma string:

```
Haskell
import Data.Text (unpack)
import qualified Data.Text as T
import Text.Regex.Posix (subRegex)

-- uma string de exemplo
texto = "Existem 123 coisas na lista."

-- substituindo números por asteriscos
novoTexto = subRegex (makeRegex "([0-9]+)") (unpack texto) "*"

unpack novoTexto -- imprime "Existem * coisas na lista."
```

## Aprofundando

Além das funções mencionadas, há muitas outras maneiras de realizar buscas e substituições de texto em Haskell. Você pode explorar as inúmeras bibliotecas disponíveis na comunidade Haskell, como `Data.String.Utils` e `Data.List.Utils`, que possuem funções úteis para manipulação de strings.

Se você estiver interessado em aprender mais sobre expressões regulares em Haskell, a documentação oficial da linguagem possui um bom tutorial sobre o assunto. Além disso, você pode explorar as funcionalidades de substituição de texto nas bibliotecas mencionadas, como `Data.Text.Regex`, para ter um melhor entendimento sobre como elas funcionam.

## Veja também

- Documentação oficial sobre expressões regulares em Haskell (em inglês): https://www.haskell.org/onlinereport/regexps.html
- Biblioteca `Data.Text.Regex` (em inglês): https://hackage.haskell.org/package/regex-base
- Biblioteca `Data.String.Utils` (em inglês): https://hackage.haskell.org/package/missingh-1.4.0.1/docs/Data-String-Utils.html
- Biblioteca `Data.List.Utils` (em inglês): https://hackage.haskell.org/package/list-utils-0.1.0/docs/Data-List-Utils.html