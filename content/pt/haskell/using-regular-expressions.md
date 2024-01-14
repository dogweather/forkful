---
title:    "Haskell: Utilizando expressões regulares."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Porque Usar Expressões Regulares?

Expressões regulares são um recurso poderoso na programação de computadores. Elas são usadas para busca e manipulação de texto de forma eficiente e concisa. Ao aprender a usar expressões regulares, você será capaz de lidar com uma variedade de tarefas de processamento de texto com facilidade e eficiência.

## Como Usar Expressões Regulares em Haskell

Em Haskell, expressões regulares são suportadas pela biblioteca "Text.Regex.Posix", que deve ser importada no início do código:

```Haskell
import Text.Regex.Posix
```

Para utilizar uma expressão regular em uma string, podemos usar a função "matchRegexPOSIX", que recebe como argumentos a expressão regular e a string a ser verificada. Por exemplo, se quisermos verificar se uma string contém apenas números, podemos usar a seguinte expressão regular:

```Haskell
matchRegexPOSIX "^[0-9]+$" "12345"
```

O resultado será "Just (0,5)" - indicando que a string é um match completo da expressão regular, começando no índice 0 e terminando no índice 5 (o índice da última letra +1). Caso a string não seja um match, o resultado será "Nothing".

Além do "matchRegexPOSIX", existem outras funções úteis para manipulação de strings com expressões regulares, como o "getAllTextMatches", que retorna todas as strings que dão match na expressão regular, e o "subRegex", que substitui partes da string que dão match na expressão regular por outra string.

## Mergulho Profundo em Expressões Regulares

Expressões regulares são baseadas em um conceito chamado de "autômatos finitos determinísticos". Essencialmente, elas funcionam como "máquinas" que leem e processam um texto de acordo com as regras definidas na expressão regular.

Para criar suas próprias expressões regulares, é importante entender os metacaracteres e suas funções. Por exemplo, o "^" indica o início da string, o "$" indica o final, o "." representa qualquer caractere e o "+" indica uma ou mais repetições. Combinando esses e outros metacaracteres, é possível criar expressões regulares complexas para as mais diversas necessidades.

## Veja Também

- [Haskell Wiki: Text/Regex](https://wiki.haskell.org/Text/Regex)
- [Haskell Documentation: Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- [RegExr: Online Regular Expression Tester](https://regexr.com/)