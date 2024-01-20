---
title:                "Pesquisando e substituindo texto"
html_title:           "Bash: Pesquisando e substituindo texto"
simple_title:         "Pesquisando e substituindo texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

---

## O Que & Por Quê?

Pesquisar e substituir texto significa encontrar um trecho de texto específico e alterá-lo para algo diferente. Os programadores fazem isso quando precisam modificar rapidamente partes específicas do código ou dados.

## Como Fazer

Aqui está um exemplo simples de como pesquisar e substituir texto em Kotlin:

```Kotlin
fun main() {
    val texto = "Bom dia, mundo!"
    val novoTexto = texto.replace("mundo", "kotlin")
    println(novoTexto)
}
```
Este programa vai imprimir: `Bom dia, kotlin!`

Utilizando expressões regulares temos outras possibilidades mais complexas. Veja:

```Kotlin
fun main() {
    val texto = "Bom dia, mundo! Hoje o mundo está lindo."
    val regex = "mundo".toRegex()
    val novoTexto = regex.replace(texto, "kotlin")
    println(novoTexto)
}
```
Aqui, o programa substitui todas as ocorrências da palavra `mundo` por `kotlin` produzindo a saída: `Bom dia, kotlin! Hoje o kotlin está lindo.`

## Aprofundando

Historicamente, as operações de pesquisa e substituição foram uma parte essencial da edição de texto e da programação, e as primeiras implementações datam das primeiras edições de texto digitais.

Existem alternativas para a função `replace()` em Kotlin, dependendo das suas necessidades. Se você precisa de mais controle sobre o processo de substituição, pode usar `replaceFirst()`, que apenas substitui a primeira ocorrência do danado do texto.

Como funciona a função `replace()` em Kotlin? Na verdade, ela usa uma estrutura de dados chamada de "matriz" e um algoritmo de correspondência de padrões para encontrar as strings que correspondem ao texto que queremos substituir. 

## Veja Também

Para obter mais detalhes sobre manipulação de strings em Kotlin, confira a documentação oficial: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/

E para entender mais sobre expressões regulares, consulte: https://developer.android.com/kotlin/regexp