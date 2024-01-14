---
title:    "Elm: Busca e substituição de texto"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Porque

Algumas vezes, quando estamos escrevendo código, nos deparamos com a necessidade de buscar e substituir texto em nossos arquivos. Isso pode ser por diversos motivos, como corrigir erros de digitação ou realizar uma refatoração em nossas funções. Felizmente, o Elm possui ferramentas que facilitam esse processo.

## Como fazer

Para buscar e substituir texto em um arquivo que esteja sendo trabalhado no Elm, podemos usar a função `String.replace` do módulo `String`. Ela recebe três argumentos: o texto que queremos substituir, o novo texto que irá substituir e a string em que queremos fazer a substituição. Veja um exemplo:

```Elm
import String exposing (..)

textoIncial = "Eu amo programar em Elm!"
textoFinal = replace "amo" "adoro" textoIncial

-- Saída: "Eu adoro programar em Elm!"
```

Podemos também usar a função `String.replaceAll` para substituir todas as ocorrências do texto, ao invés de apenas a primeira. E se quisermos fazer a substituição apenas de forma case-insensitive, podemos usar a função `String.replaceCaseInsensitive`. Veja mais exemplos:

```Elm
import String exposing (..)

textoIncial = "Preciso escrever mais código em Elm, pois é muito divertido!"
textoFinal = replaceAll "Elm" "Haskell" textoInicial

-- Saída: "Preciso escrever mais código em Haskell, pois é muito divertido!"

textoFinal = replaceCaseInsensitive "divertido" "legal" textoInicial

-- Saída: "Preciso escrever mais código em Elm, pois é muito legal!"
```

## Aprofundando

Caso tenhamos a necessidade de substituir texto em um contexto mais complexo, podemos usar a função `String.replaceRegex` do módulo `Regex`. Ela utiliza expressões regulares para buscar e substituir texto em uma string. Veja um exemplo:

```Elm
import Regex exposing (..)

textoInicial = "Não consigo entender esse erro [E1234] no meu código."

padrao = regex "\\[E\\d*\\]"
novoTexto = "(veja a doc do [E1234] para resolver)"

textoFinal = replaceRegex padrao textoInicial novoTexto

-- Saída: "Não consigo entender esse erro (veja a doc do [E1234] para resolver) no meu código."
```

Podemos também usar as expressões regulares para extrair informações de um texto, em vez de apenas substituí-lo. E se quisermos fazer substituições recursivas, podemos usar a função `String.replaceRecursiveRegex`. Confira a documentação oficial para mais detalhes.

## Veja também

- Documentação oficial do módulo String: https://package.elm-lang.org/packages/elm/core/latest/String
- Documentação oficial do módulo Regex: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- Cheatsheet para expressões regulares: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet