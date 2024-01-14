---
title:                "Kotlin: Convertendo uma string para minúsculo"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Ao trabalhar com programação, é comum ter a necessidade de manipular strings de diferentes maneiras. Uma dessas maneiras é converter uma string para minúsculas. Isso pode ser útil em vários cenários, como validar senhas ou nomes de usuário, comparar strings de forma não case-sensitive ou simplesmente para melhorar a legibilidade do código.

## Como fazer isso em Kotlin

A conversão de uma string para minúsculas em Kotlin é muito simples e pode ser feita de diferentes maneiras. Aqui estão algumas opções:

```Kotlin
val string = "Olá Mundo!"
val lowerCaseString = string.toLowerCase()

println(lowerCaseString) // saída: olá mundo!
```

Também é possível utilizar a função `toLowerCase()` diretamente em uma string:

```Kotlin
val string = "Olá Mundo!"

println(string.toLowerCase()) // saída: olá mundo!
```

Além disso, se você quiser converter uma única letra para minúscula, pode usar o método `toLowercase()` da classe `Char`:

```Kotlin
val letter = 'A'
val lowerCaseLetter = letter.toLowercase()

println(lowerCaseLetter) // saída: a
```

## Mergulho profundo

É importante ressaltar que a conversão para minúsculas pode variar dependendo da configuração de idioma do sistema onde o código está sendo executado. Em sistemas que possuem acentos, por exemplo, o resultado pode ser diferente do esperado. Para evitar esse tipo de problema, é possível utilizar o método `toLowercase(Locale)` e especificar o idioma desejado:

```Kotlin
val string = "Olá Mundo!"

println(string.toLowerCase(Locale("PT"))) // saída: olá mundo!
```

Além disso, é importante lembrar que essa conversão não altera a string original, ela apenas retorna uma nova string com o resultado da conversão.

Outro ponto a ser levado em conta é que esse método não afeta números ou caracteres especiais, ele apenas converte letras maiúsculas em minúsculas.

## Veja também

- Documentação oficial do Kotlin sobre `toLowercase()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lowercase.html
- Tutorial sobre manipulação de strings em Kotlin: https://medium.com/@jonathanboccara/kotlin-strings-on-steroids-eb9476f9ba51