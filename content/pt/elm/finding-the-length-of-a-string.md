---
title:                "Elm: Encontrando o comprimento de uma string."
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que

Ao trabalhar com programação, muitas vezes nos deparamos com a necessidade de encontrar o tamanho de uma string. Isso pode ser útil em diversas situações, como validação de dados ou manipulação de texto.

# Como fazer

Para encontrar o comprimento de uma string em Elm, podemos utilizar a função "length" da biblioteca String. Veja um exemplo:

```
import String

titulo = "Eu amo programar em Elm"
tamanho = String.length titulo

Html.text (String.fromInt tamanho) -- Output: 26
```

Como podemos ver, a função "length" retorna um inteiro que representa o comprimento da string fornecida. Neste caso, a string "Eu amo programar em Elm" possui 26 caracteres.

Também é possível encontrar o tamanho de uma string com a notação de ponto. Veja outro exemplo:

```
titulo = "Eu amo programar em Elm"
tamanho = titulo.length

Html.text (String.fromInt tamanho) -- Output: 26
```

# Mergulho profundo

Ao trabalhar com a função "length" em Elm, é importante lembrar que ela conta o número de caracteres, incluindo espaços em branco e caracteres especiais. Por exemplo, se utilizarmos a string "Eu amo Elm", o resultado será 9, incluindo o espaço entre "amo" e "Elm".

Também é importante notar que, assim como em outras linguagens de programação, a indexação de strings em Elm começa no número 0. Isso significa que o primeiro caractere de uma string terá o índice 0, o segundo terá o índice 1 e assim por diante.

Por fim, é possível utilizar a função "length" em conjunto com outras funções da biblioteca String, como "slice" e "concat", para manipulação e formatação de strings de maneira eficiente.

# Veja também

- Documentação da função "length" em Elm: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Tutorial de Elm em português: https://devsquad.com.br/tutorial-de-elm-em-portugues/