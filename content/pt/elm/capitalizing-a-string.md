---
title:                "Capitalizando uma string"
date:                  2024-01-19
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalizar uma string significa converter todos os caracteres da string para maiúsculas. Programadores fazem isso para normalizar dados para comparação ou para fins estéticos, como em títulos ou cabeçalhos.

## Como Fazer:

Elm não possui uma função embutida para capitalizar todas as letras de uma string, mas podemos criar uma função para isso:

```Elm
import String exposing (toUpper)

capitalize : String -> String
capitalize str =
    toUpper str

-- Exemplo de uso:
main =
    String.words "olá, mundo!"
        |> List.map capitalize
        |> String.join " "
        |> Html.text
```

Saída esperada:
```
"OLÁ, MUNDO!"
```

## Mergulho Profundo:

Em alguns contextos, capitalizar strings é simples como no exemplo acima. No entanto, linguagens diferentes têm casos especiais (como caracteres acentuados ou cedilha), e a função `toUpper` pode não cobrir todos esses casos se baseando unicamente nas regras do idioma inglês.

Uma alternativa é usar bibliotecas que entendem a localização para garantir uma capitalização apropriada em diversos idiomas. A implementação de capitalizar strings geralmente envolve mapear cada caractere para sua forma maiúscula de acordo com uma tabela de caracteres, como a Unicode.

Na história da programação, funções de manipulação de strings sempre foram essenciais. Em linguagens de mais baixo nível, essas operações podiam ser mais complexas, exigindo manipulação direta de bytes. Em Elm, graças a funções de alto nível como `toUpper`, o processo é muito mais direto.

## Veja Também:

- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode Character Table](https://www.unicode.org/charts/)

Lembre-se de verificar a documentação oficial do Elm para qualquer atualização em métodos de manipulação de strings e considere explorar pacotes adicionais para suporte avançado a localização se suas necessidades de capitalização forem além dos exemplos simples.
