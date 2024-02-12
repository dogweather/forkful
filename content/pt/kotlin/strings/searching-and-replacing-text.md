---
title:                "Pesquisando e substituindo texto"
aliases:
- /pt/kotlin/searching-and-replacing-text/
date:                  2024-01-20T17:58:19.172800-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Buscar e substituir texto é mudar palavras ou sequências de caracteres específicas por outras em uma string. Programadores fazem isso para corrigir erros, atualizar dados ou alterar códigos de forma rápida e eficiente.

## Como Fazer:
```kotlin
fun main() {
    val texto = "As raposas são astutas e as corujas são sábias."
    val pesquisa = "raposas"
    val substituicao = "linces"

    val textoAtualizado = texto.replace(pesquisa, substituicao)
    println(textoAtualizado) // As linces são astutas e as corujas são sábias.
}
```

Outro exemplo, com expressões regulares:
```kotlin
fun main() {
    val texto = "Use Kotlin versão 1.3 e atualize para 1.4 depois."
    val regex = "1\\.\\d".toRegex()

    val textoAtualizado = regex.replace(texto, "1.5")
    println(textoAtualizado) // Use Kotlin versão 1.5 e atualize para 1.5 depois.
}
```

## Mergulho Profundo:
Historicamente, a busca e substituição de texto vem do tempo das máquinas de escrever e da edição de texto impresso, onde correções eram feitas manualmente. Com o advento dos computadores e editores de texto, essa função se tornou essencial para editar códigos e documentos de forma digital.

Existem alternativas ao método `replace` padrão, como a manipulação de strings com loops e condições ou o uso de bibliotecas externas para casos mais complexos. No entanto, o método `replace` e os recursos de expressões regulares do Kotlin são suficientemente poderosos para a maioria das tarefas de busca e substituição de texto.

O método `replace` pode ser direto quando apenas caracteres específicos são procurados. Com expressões regulares, adicionamos flexibilidade para padrões mais complexos. É importante entender bem expressões regulares para usá-las efetivamente sem efeitos colaterais indesejados.

## Veja Também:
- [Guia sobre expressões regulares em Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Tutorial Interativo de Expressões Regulares](https://regexone.com/)
