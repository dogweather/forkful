---
title:    "Kotlin: Concatenando strings"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que utilizar a concatenação de strings?

A concatenação de strings é uma função essencial na programação de Kotlin, pois permite combinar diversas strings em uma única string. Isso é útil quando se deseja criar mensagens personalizadas, strings de consulta para bancos de dados ou até mesmo para criar URLs.

## Como utilizar a concatenação de strings

Para utilizar a concatenação de strings em Kotlin, basta usar o operador "+" entre as strings desejadas dentro de um método print ou println. Por exemplo:

```Kotlin
println("Olá," + "maravilhosos leitores!")
```

A saída desse código seria "Olá, maravilhosos leitores!".

Além disso, também é possível utilizar o método "plus()" para concatenar strings em Kotlin. Por exemplo:

```Kotlin
val saudacao = "Olá,".plus("maravilhosos leitores!")
println(saudacao)
```

A saída seria a mesma que no exemplo anterior.

## Mergulho profundo na concatenação de strings

Além da utilização básica do operador "+" ou do método "plus()", existem outras formas de concatenar strings em Kotlin. Uma delas é através do método "format()", que permite substituir os marcadores de posição na string por valores específicos. Por exemplo:

```Kotlin
val nome = "Fulano"
val sobrenome = "da Silva"
println("Olá, %s %s!".format(nome, sobrenome))
```

A saída seria "Olá, Fulano da Silva!".

Outra forma é utilizando a função "StringBuilder". Essa função é mais eficiente quando se deseja concatenar muitas strings, uma vez que não cria um novo objeto a cada concatenação. Por exemplo:

```Kotlin
val mensagem = StringBuilder("Olá, ")
    .append("maravilhosos")
    .append(" leitores!")
    .toString()
println(mensagem)
```

A saída seria a mesma do primeiro exemplo.

## Veja também

- Documentação oficial do Kotlin sobre concatenação de strings: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Tutorial de Kotlin sobre concatenação de strings: https://www.javatpoint.com/kotlin-string-formatting
- Vídeo explicando as diferentes formas de Concatenar Strings em Kotlin: https://www.youtube.com/watch?v=3vp7MLgypxY