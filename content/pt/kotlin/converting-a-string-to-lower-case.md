---
title:                "Convertendo uma string para minúsculas"
html_title:           "Kotlin: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Convertendo uma string para letras minúsculas é um processo comumente usado por programadores para alterar o formato de texto em seus programas. Isso permite que eles trabalhem com textos uniformes e padronizados, independentemente de como eles foram inseridos pelo usuário ou pelo sistema.

## Como:

```
fun main() {
    val texto = "UM TEXTO QUALQUER"
    println(texto.toLowerCase())
}

// Output:
// um texto qualquer
```

```
fun main() {
    val texto = "caracteresMaiusculos"
    println(texto.toLowerCase())
}

// Output:
// caracteresmaiusculos
```

## Mergulho Profundo:

Existem várias maneiras de converter uma string para letras minúsculas em Kotlin, incluindo o uso do método `toLowerCase()` como mostrado acima ou o uso do operador de atribuição `+=` com a função de extensão `lowercase()`.

Embora essa conversão possa parecer simples, ela tem uma longa história e evolução na programação. Antigamente, em idiomas como o COBOL, as letras maiúsculas eram usadas para indicar comandos e as minúsculas para dados. Com o tempo, e especialmente com o uso de computadores pessoais, a distinção se tornou menos necessária e o uso de strings totalmente em minúsculas se tornou uma prática mais comum.

Além disso, existem alternativas para converter strings em letras minúsculas, como o uso de expressões regulares ou bibliotecas externas. No entanto, a solução nativa da linguagem Kotlin é mais adequada e eficiente em muitos casos.

## Veja Também:

- [Documentação oficial do método toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Tutorial sobre strings em Kotlin](https://www.tutorialspoint.com/kotlin/kotlin_string.htm)
- [Artigo sobre strings e suas funções em Kotlin](https://www.geeksforgeeks.org/kotlin-strings/)