---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

# A deletar caracteres de acordo com um padrão no Elm

## O Que é & Por Que?

Deletar caracteres de acordo com um padrão é o ato de remover certos caracteres de uma string baseando-se numa condição. Programadores fazem isso quando querem limpar ou formatar dados.

## Como fazer:

Vamos ver um exemplo simples de como deletar caracteres de uma string no Elm.

```Elm
import String exposing (left, dropLeft)

removeChar : Char -> String -> String
removeChar x input =
    let
        split = String.split (String.fromChar x) input
    in
        String.join "" split
```

Você pode chamar a função `removeChar` passando o caractere a ser removido e a string de onde ele será apagado, como exemplificado a seguir:

```Elm
removeChar 'a' "banana" 
-- A saída será "bnn"
```

## Deep Dive:

Uma função muito utilizada para remover caracteres baseando-se em um padrão é a função `String.split`. Neste caso, o padrão é determinado pelo primeiro argumento da função, e os caracteres correspondentes ao padrão são removidos da string no segundo argumento. 

Em versões anteriores do Elm, essa funcionalidade não estava embutida na biblioteca padrão, fazendo com que os programadores tivessem que criar suas próprias funções personalizadas. 

Em relação às alternativas, outras linguagens de programação têm funções similares, como  `replaceAll()` em JavaScript e  `gsub()` em Ruby.

## Veja Também:

Você pode encontrar mais informações e utilizações da função `String.split` em:

- Documentação oficial do Elm: [https://package.elm-lang.org/packages/elm/core/latest/String#split](https://package.elm-lang.org/packages/elm/core/latest/String#split)