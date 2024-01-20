---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Concatenar strings é o processo de juntar duas ou mais strings em uma única. Fazemos isso para construir ou manipular textos de maneira dinâmica em nossos programas.

## Como Fazer:

Aqui estão algumas maneiras de concatenar strings em Kotlin:

### Método 1: Usando o operador (+)

```Kotlin
val string1 = "Olá, "
val string2 = "Mundo!"
val resultado = string1 + string2
println(resultado) // "Olá, Mundo!"
```
### Método 2: Usando a função concat()

```Kotlin
val string1 = "Olá, "
val string2 = "Mundo!"
val resultado = string1.concat(string2)
println(resultado) // "Olá, Mundo!"
```

### Método 3: Usando string templates

```Kotlin
val nome = "Mundo"
val resultado = "Olá, $nome!"
println(resultado) // "Olá, Mundo!"
```

## Fundo:

Concatenar strings é uma operação fundamentalmente importante em programação. Tem sido uma funcionalidade disponível mesmo nas primeiras linguagens de programação.

Kotlin oferece várias maneiras de concatenar strings, cada uma com sua característica. O operador (+) é o mais simples e direto. A função concat() é robusta, mas pode ser mais lenta para grandes volumes de dados. As templates de string são uma inovação do Kotlin, que permitem que as variáveis sejam incorporadas diretamente dentro de uma string.

Há também alternativas, como StringBuffer e StringBuilder, para casos de uso avançados, como manipulação pesada de strings ou melhorias de desempenho.

## Veja Também:

- [Documentação Oficial Kotlin - Strings] (https://kotlinlang.org/docs/reference/basic-types.html#string-literals)
- [Guia JetBrains Kotlin - Concatenação de Strings] (https://www.jetbrains.com/help/idea/concatenate-strings.html)
- [Kotlin para iniciantes: Concatenação de Strings] (https://www.geeksforgeeks.org/kotlin-string-concatenation/)