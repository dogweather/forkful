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

# O que e por que?

Capitalize uma string significa tornar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para tornar o texto mais legível e bem formatado em suas aplicações.

## Como fazer:

```Kotlin
fun capitalizarString(palavra: String): String {
    val palavras = palavra.split(" ")
    var stringCapitalizada = ""
    for (palavra in palavras) {
        stringCapitalizada += "${palavra.capitalize()} "
    }
    return stringCapitalizada.trim()
}

println(capitalizarString("exemplo de texto em kotlin"))
// Output: Exemplo De Texto Em Kotlin
```

## Profundando:

Historicamente, capitalizar strings era uma forma de destacar nomes próprios e títulos em textos manuscritos. Atualmente, essa prática é amplamente utilizada em programação para melhorar a legibilidade e organização de códigos, especialmente em situações onde é necessário exibir o texto em maiúsculas, como em cabeçalhos ou títulos de botões.

Uma alternativa ao método acima é usar a função `replace()` para substituir a primeira letra de cada palavra pela versão capitalizada, como mostrado no exemplo abaixo:

```Kotlin
"exemplo de texto em kotlin".replaceEachChar { if (isFirst()) it.titlecase(Locale.getDefault()) else it.toString() }
```

Em termos de implementação, a função `capitalize()` é internamente chamada pelo método `titlecase()` para capitalizar a primeira letra de cada palavra, enquanto a função `replaceEachChar()` percorre a string e aplica a lógica definida pelo usuário para cada caractere.

## Veja também:

- [Documentação da função `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Alternativas para capitalizar strings em Kotlin](https://www.techiedelight.com/capitalize-strings-kotlin/)