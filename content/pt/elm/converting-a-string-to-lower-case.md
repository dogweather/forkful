---
title:                "Convertendo uma string para minúsculas"
date:                  2024-01-20T17:38:07.165801-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Converter uma string para letras minúsculas significa transformar todas as letras maiúsculas da string em sua forma minúscula correspondente. Programadores fazem isso para padronizar dados para comparação, pesquisa ou para atender regras de formatação.

## Como fazer:

```Elm
import String

lowercaseString : String -> String
lowercaseString str =
    String.toLower str

-- Uso da função
main =
    String.toLower "OLÁ, MUNDO!"

-- Saída: "olá, mundo!"
```

## Mergulho Profundo

Converter strings para minúsculas é uma prática comum desde os primeiros dias da computação, facilitando a comparação de strings independentemente da formatação inicial. No Elm, `String.toLower` é a função embutida para realizar essa tarefa, e ela cobre a maioria dos casos de uso com eficácia. Alternativamente, poderíamos implementar nossa própria função, mas isso não é recomendado já que a função padrão lida bem com unicode e outros edge cases. A implementação por baixo dos panos de `String.toLower` depende do runtime do Elm e como ele se relaciona com JavaScript, aproveitando o método `toLowerCase()` de strings do próprio JavaScript, garantindo compatibilidade e performance.

## Veja Também

- Documentação oficial Elm `String.toLower`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Unicode e Elm: como o Elm lida com caracteres Unicode: https://elm-lang.org/docs/strings
- Artigo sobre como a função `toLowerCase()` funciona em JavaScript (relevante já que Elm compila para JavaScript): https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
