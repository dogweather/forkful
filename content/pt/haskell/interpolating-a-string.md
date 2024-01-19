---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolação de Strings em Haskell: O Que É e Como Utilizar

## O Que & Por Quê?

Interpolação de string é o ato de substituir placeholders por valores em uma string. Programadores fazem isso para construir strings de maneira mais legível e eficaz.

## Como Fazer:

Haskell não suporta interpolação de string out-of-the-box. Mas, com a biblioteca "Text.Printf", isso é possível. Veja abaixo:

```Haskell
import Text.Printf

nome = "João"
idade = 25

main = printf "Olá %s, você tem %d anos.\n" nome idade
```
A saída será: `Olá João, você tem 25 anos.`

## Mergulho Profundo

Haskell, sendo uma das mais antigas linguagens de programação funcional, foi desenvolvida em uma época em que interpolação de strings não era muito comum. Por isso, ela não suporta nativamente esse recurso. A biblioteca "Text.Printf", contudo, oferece esse recurso.

É válido mencionar a biblioteca "interpolate" que oferece uma abordagem mais moderna e intuitiva:

```Haskell
import Data.String.Interpolate

nome = "João"
idade = 25

main = putStrLn [i|Olá #{nome}, você tem #{idade} anos.|]
```

Note que a biblioteca "interpolate" utiliza o operador `#{}`, mais parecido com outras linguagens modernas como Ruby e JavaScript.

## Veja Também:

1. Documentação oficial do Haskell "Text.Printf": http://hackage.haskell.org/package/base-4.14.1.0/docs/Text-Printf.html
2. Biblioteca "Interpolate" no Hackage: https://hackage.haskell.org/package/interpolate
3. Post no Stack Overflow sobre a interpolação de strings em Haskell: https://stackoverflow.com/questions/17422209/is-there-a-string-interpolation-function-in-haskell