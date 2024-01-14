---
title:                "Haskell: Procurando e substituindo texto"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que procurar e substituir texto em Haskell?

A busca e substituição de texto pode ser uma tarefa tediosa e demorada quando realizada manualmente. No entanto, em linguagens de programação funcional, como Haskell, podemos aproveitar o poder e a facilidade da programação para automatizar e simplificar esse processo. Além disso, a busca e substituição de texto é especialmente útil em casos onde temos um grande volume de dados para processar.

## Como fazer em Haskell

Haskell possui uma função útil chamada `replace`, que nos permite substituir uma sequência de caracteres por outra em uma string. Vejamos um exemplo:

```
replace :: ByteString -> ByteString -> ByteString -> ByteString
replace old new target = intercalate new . split (string old) $ target
```

Neste exemplo, utilizamos o pacote `bytestring` para lidar com as strings em bytes. A função `replace` recebe três parâmetros: a sequência de caracteres que queremos substituir, a nova sequência de caracteres e a string em que desejamos realizar a substituição. Em seguida, utilizamos a função `intercalate` para unir as strings resultantes após a substituição e a função `split` para separar a string em uma lista de strings, utilizando a sequência de caracteres original como delimitador.

Suponha que temos a seguinte string:

```
"Olá, vamos substituir algumas vogais?"
```

Se desejarmos substituir todas as vogais por asteriscos, podemos fazer da seguinte forma:

```
replace "aeiou" "*" "Olá, vamos substituir algumas vogais?"
```

O resultado seria:

```
"Ol*, v*m*s s*bst*t**r *lg*n*s v*g**s?"
```

Note que as vogais foram substituídas pela sequência de caracteres "*" conforme esperado.

## Mergulho Profundo

Além da função `replace`, Haskell possui várias outras funções e ferramentas que podem ser úteis para realizar busca e substituição de texto. Algumas delas são:

- A função `replaceAll`, do pacote `text`, que recebe uma expressão regular para buscar e uma string substituta;
- O módulo `Text.Regex.TDFA`, que permite utilizar expressões regulares para busca e substituição;
- A ferramenta `sed`, disponível através do pacote `shelly`, que permite realizar busca e substituição em arquivos utilizando expressões regulares.

## Veja Também

- [Documentação da função `replace`](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#v:replace)
- [Documentação da função `replaceAll`](https://hackage.haskell.org/package/text/docs/Data-Text-Internal.html#v:replaceAll)
- [Documentação do módulo `Text.Regex.TDFA`](https://hackage.haskell.org/package/regex-tdfa/docs/Text-Regex-TDFA.html)
- [Documentação do pacote `shelly`](https://hackage.haskell.org/package/shelly/docs/Shelly.html)