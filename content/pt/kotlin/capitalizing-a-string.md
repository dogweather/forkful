---
title:                "Capitalizando uma string"
date:                  2024-01-19
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Capitalizar uma string significa transformar todas as letras iniciais de palavras em maiúsculas, ou apenas a primeira letra da string inteira. Programadores fazem isso para formatar textos, como nomes próprios ou títulos, seguindo padrões de legibilidade e estilo.

## Como Fazer:

Capitalizar a primeira letra:

```kotlin
fun main() {
    val texto = "kotlin é legal!"
    val capitalizado = texto.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(capitalizado)
}
// Saída: Kotlin é legal!
```

Capitalizar todas as palavras:

```kotlin
fun main() {
    val texto = "kotlin é muito legal!"
    val capitalizado = texto.split(" ").joinToString(" ") { it.capitalize() }
    println(capitalizado)
}
// Saída: Kotlin É Muito Legal!
```

## Aprofundamento

Historicamente, a função `capitalize()` era usada para capitalizar a primeira letra de strings no Kotlin, mas a partir do Kotlin 1.5, ela foi substituída por `replaceFirstChar`, pois a função original estava limitada ao idioma inglês e não tratava adequadamente os caracteres Unicode. Além disso, existem diversas bibliotecas Java que também podem ser usadas em Kotlin para capitalizar strings, como a Apache Commons Lang. A implementação de capitalização em Kotlin pode variar, pois alguns idiomas têm regras específicas para capitalização que podem não ser seguidas utilizando métodos simples.

## Veja Também

- Documentação oficial da Kotlin Standard Library sobre manipulação de strings: [Kotlinlang - Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/)
- Projeto Apache Commons Lang para manipulação de strings: [Apache Commons Lang - StringUtils](https://commons.apache.org/proper/commons-lang/javadocs/api-release/org/apache/commons/lang3/StringUtils.html)
- Aprofunde seus conhecimentos sobre os caracteres Unicode e suas particularidades na capitalização: [Unicode Character Database](https://www.unicode.org/ucd/)
