---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

# Trabalhando com Substrings em Kotlin

O mundo da programação está sempre em constante mudança e evolução, e Kotlin não é uma exceção. Hoje vamos explorar uma funcionalidade crítica e útil: a extração de substrings.

## O Que & Por Quê?
A extração de substrings é o ato de obter partes específicas de uma string. Os programadores fazem isso para manipular e analisar dados, geralmente em tarefas que envolvem processamento de texto.

## Como fazer:
Kotlin faz com que a extração de substrings seja direta. Olhe para o código a seguir:

```Kotlin
fun main() {
    val str = "Olá, Mundo Kotlin!"
    println(str.substring(0, 4)) // "Olá," 
    println(str.substring(5, 10)) // "Mundo"
}
```
Bem simples, não é? O método `.substring()` pega dois argumentos: o índice inicial e o final. A substring retornada vai do índice inicial até um caractere antes do índice final.

Mas e se quisermos usar uma função que pegue uma substring do índice inicial até o final da string? Isso também é fácil.

```Kotlin
fun main() {
    val str = "Olá, Mundo Kotlin!" 
    println(str.substring(5)) // "Mundo Kotlin!"
}
```

### Deep Dive
A extração de substrings é uma técnica antiga, remontando aos primeiros dias da programação. Em Kotlin, as funções `substring` fazem parte da classe `String`.

Existem alternativas para a extração de substrings. Você pode usar expressões regulares ou até mesmo percorrer a string manualmente - mas isso é geralmente mais complicado e menos eficiente.

No que diz respeito a detalhes de implementação, Kotlin usa os índices de string 0-based, o que significa que o primeiro caractere está no índice 0. Além disso, a string original não é alterada quando uma substring é extraída - uma nova string é criada.

## Veja Também
- [Tutorial Rápido Kotlin - Substrings](https://www.tutorialkart.com/kotlin/kotlin-substring/)
- [Trabalhando com Strings em Kotlin - Medium](https://medium.com/@elizarov/kotlin-strings-1c19d26a948b)