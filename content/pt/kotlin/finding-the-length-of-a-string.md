---
title:                "Kotlin: Encontrando o comprimento de uma string"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que encontrar o comprimento de uma string?

Encontrar o comprimento de uma string é um conceito básico na programação e pode ser útil em várias situações. Saber como fazer isso pode facilitar a manipulação e análise de dados em diferentes projetos.

## Como fazer:

```Kotlin
val palavra = "programação"
val comprimento = palavra.length

println(comprimento) // output: 12
```

No exemplo acima, criamos uma variável chamada "palavra" que contém a string "programação". Em seguida, usamos a função "length" para encontrar o comprimento da string e armazenamos o resultado na variável "comprimento". Finalmente, imprimimos o valor de "comprimento" e obtemos o valor 12, que é o comprimento da string "programação".

## Mergulho Profundo:

Ao encontrar o comprimento de uma string, deve-se ter em mente que o valor retornado pela função "length" é o número de caracteres da string, incluindo espaços em branco. Isso pode ser útil ao criar lógicas condicionais ou ao formatar saídas de dados.

Além disso, no Kotlin, a função "length" é uma propriedade da classe String, o que significa que podemos usá-la diretamente em qualquer string, sem precisar importar bibliotecas externas.

## Veja também:

- [Documentação oficial do Kotlin sobre a propriedade "length"](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/length.html)
- [Exemplos práticos de uso da função "length"](https://www.tutorialspoint.com/kotlin-string-length-function)
- [Vídeo tutorial sobre a função "length" em Kotlin](https://www.youtube.com/watch?v=RSUuPpTdHRM)

O conhecimento sobre como encontrar o comprimento de uma string é fundamental para qualquer programador, independentemente da linguagem de programação utilizada. Espero que este artigo tenha sido útil para você entender melhor esse conceito. Até a próxima!