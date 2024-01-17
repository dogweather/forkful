---
title:                "Encontrando o comprimento de uma string."
html_title:           "Kotlin: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O que e por quê?

Encontrar o tamanho de uma string é uma tarefa comum na programação. Isso nos permite saber quantos caracteres uma determinada string contém, o que é útil para diversas aplicações. Programadores muitas vezes precisam encontrar o tamanho de uma string para validar entradas do usuário, formatar saídas, entre outras ações.

## Como fazer:

Para encontrar o tamanho de uma string em Kotlin, você pode usar a função length(). Veja um exemplo de código em Kotlin abaixo:

```Kotlin
val string = "Olá mundo!"
val tamanho = string.length()
print(tamanho) // Saída: 11
```

## Mergulho profundo:

Encontrar o tamanho de uma string é uma tarefa que tem sido realizada desde os primeiros dias da programação. No início, as linguagens de programação tinham funções específicas para encontrar o tamanho de uma string, mas hoje em dia a função length() é comum na maioria das linguagens orientadas a objetos, incluindo Kotlin.

Existem algumas alternativas para encontrar o tamanho de uma string, como contar os caracteres manualmente ou usar a propriedade size, que retorna o tamanho de uma coleção de dados. No entanto, a função length() em Kotlin é a forma mais recomendada e prática de encontrar o tamanho de uma string.

## Veja também:

- [Documentação oficial do Kotlin sobre a função length()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html)
- [Outros métodos para obter o tamanho de uma string em Kotlin](https://www.tutorialkart.com/kotlin/measure-string-length-in-kotlin/)