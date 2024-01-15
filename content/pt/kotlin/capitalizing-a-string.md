---
title:                "Capitalizando uma string"
html_title:           "Kotlin: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Você já se deparou com a necessidade de capitalizar uma string em seus projetos de programação? Se sim, então este artigo é para você! Aprenda como capitalizar uma string de forma eficiente usando Kotlin e dê um toque especial aos seus códigos.

## Como Fazer

Começaremos com um exemplo simples, explicando o passo a passo de como capitalizar uma string em Kotlin:

```Kotlin
fun capitalizarString(string: String): String { // Declaração da função
    return string.capitalize() // Uso do método capitalize() para capitalizar a string
}

val stringExemplo = "exemplo de texto" // String original
val stringCapitalizada = capitalizarString(stringExemplo) // Chamando a função com a string original como argumento
println(stringCapitalizada) // Output: "Exemplo de texto"
```

Parece fácil, não é? Em apenas algumas linhas de código, temos uma função que capitaliza uma string e nos retorna o resultado desejado. Mas o que está realmente acontecendo por trás dos bastidores?

## Detalhes da Capitalização de String

Quando usamos o método `capitalize()`, Kotlin usa as regras das convenções de nomenclatura em inglês para transformar a primeira letra de cada palavra em maiúscula. No entanto, isso pode não funcionar para todos os idiomas.

Por exemplo, no idioma turco, a letra "i" maiúscula é escrita como "İ", com um ponto na parte superior da letra. Se usarmos o método `capitalize()` em uma string com uma palavra em turco, esse ponto não será adicionado automaticamente e a capitalização ficará incorreta.

Para resolver esse problema, Kotlin possui outro método chamado `capitalize(Locale)` que nos permite especificar a localidade do idioma desejado. Vamos ver como usar esse método em um exemplo:

```Kotlin
fun capitalizarString(string: String): String { 
    return string.capitalize(Locale("tr", "TR")) // Adicionamos a localidade do idioma turco (Turquia)
}

val stringExemplo = "türkiye" // String original em turco
val stringCapitalizada = capitalizarString(stringExemplo)
println(stringCapitalizada) // Output: "Türkiye"
```

Agora sim, a capitalização está correta para o idioma turco!

## Veja Também

- [Documentação oficial do Kotlin sobre a função `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Mais informações sobre as regras de convenções de nomenclatura em inglês](https://medium.com/@jamesdawn/simple-guide-to-code-convention-camel-case-pascal-case-hungarian-notation-ace2b338e86e)