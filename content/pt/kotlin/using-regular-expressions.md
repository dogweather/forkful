---
title:                "Utilizando expressões regulares"
date:                  2024-01-19
html_title:           "Bash: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Expressões regulares, ou regex, são ferramentas para encontrar padrões em texto. Programadores as utilizam para buscar, substituir ou validar dados de forma eficiente.

## How to:

```kotlin
fun main() {
    val texto = "O Kotlin é divertido! Versão 2023."

    // Procurar por palavras que começam com "d"
    val regex = Regex("\\bd\\w+")
    val resultado = regex.find(texto)
    println(resultado?.value)  // Saída: "divertido"

    // Substituir "divertido" por "incrível"
    val textoAtualizado = texto.replace(regex, "incrível")
    println(textoAtualizado)  // Saída: "O Kotlin é incrível! Versão 2023."

    // Validar formato de data (DD/MM/AAAA)
    val regexData = Regex("\\b\\d{2}/\\d{2}/\\d{4}\\b")
    val dataValida = "15/10/2023"
    val dataInvalida = "32/13/2023"

    println(regexData.matches(dataValida))    // Saída: true
    println(regexData.matches(dataInvalida))  // Saída: false
}
```

## Deep Dive
Expressões regulares surgiram nos anos 1950 com o matemático Stephen Kleene. Alternativas incluem parsers de strings e linguagens específicas de domínio (DSLs), mas são mais complexas pra implementar. A biblioteca `kotlin.text` oferece suporte a regex com classes como `Regex` e funções como `matches`, `find`, e `replace`.

## See Also

- [Documentação Oficial Kotlin - Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [w3schools - Regex Tutorial](https://www.w3schools.com/java/java_regex.asp)
- [Regex101 - Teste suas Expressões Regulares](https://regex101.com/)
