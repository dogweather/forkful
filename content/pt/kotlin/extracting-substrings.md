---
title:                "Kotlin: Extraindo Substrings"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings em Kotlin

Extrair substrings é uma parte importante do desenvolvimento de aplicativos em Kotlin. É útil quando você precisa manipular strings em seu código, como cortar partes de uma string mais longa ou encontrar um determinado padrão dentro dela. Saber como extrair substrings em Kotlin pode economizar tempo e tornar seu código mais eficiente.

## Como fazer

Para extrair uma substring em Kotlin, é necessário especificar a posição inicial e final da substring desejada dentro da string original. Isso é feito usando o método `substring ()`:

```Kotlin
val string = "Olá mundo!"
val substring = string.substring(4, 9)
print(substring) // saída: mundo
```

No código acima, especificamos a posição inicial como 4 e a posição final como 9, o que nos deu a substring "mundo" da string original "Olá mundo!".

Também é possível extrair substrings a partir de uma posição inicial específica até o final da string original, omitindo o segundo argumento do método `substring ()`:

```Kotlin
val string = "Olá mundo!"
val substring = string.substring(4)
print(substring) // saída: mundo!
```

Além disso, o método `substring ()` também aceita valores negativos como argumentos, que serão contados a partir do final da string:

```Kotlin
val string = "Olá mundo!"
val substring = string.substring(4, -1)
print(substring) // saída: mundo
```

## Mergulho profundo

Além do método `substring ()`, Kotlin também oferece outras opções para extrair substrings, como o método `subSequence ()`. Este método retorna uma `CharSequence`, que pode ser facilmente convertida em uma string usando o método `toString ()`:

```Kotlin
val string = "Olá mundo!"
val substring = string.subSequence(4, 9).toString()
print(substring) // saída: mundo
```

Além disso, é possível usar o operador de indexação `[]` em uma string para extrair caracteres específicos ou até mesmo uma substring:

```Kotlin
val string = "Olá mundo!"
val character = string[4]
print(character) // saída: m
```

## Veja também

Aqui estão alguns links úteis para saber mais sobre a extração de substrings em Kotlin:

- Documentação oficial do `substring ()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html
- Documentação oficial do `subSequence ()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/sub-sequence.html
- Documentação oficial do indexador `[]`: https://kotlinlang.org/docs/reference/basic-types.html#strings