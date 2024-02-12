---
title:                "Excluindo caracteres que correspondem a um padrão"
aliases:
- /pt/kotlin/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:52.883098-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Remover caracteres que correspondem a um padrão é uma operação de filtragem de strings para moldá-las conforme a necessidade. Programadores fazem isso para limpar dados, validar entradas, ou preparar textos para processamento.

## Como Fazer:
```kotlin
fun main() {
    val textoOriginal = "Ko12tlin é 34legal!"
    val padrao = "\\d+".toRegex() // Padrão para detectar dígitos

    val textoSemDigitos = textoOriginal.replace(padrao, "")
    println(textoSemDigitos)
}
```
Output:
```
Kotlin é legal!
```

Outro exemplo:
```kotlin
fun main() {
    val informacaoSensivel = "Usuario: user123; Senha: pass456"
    val padraoSensivel = "[0-9]+".toRegex() 

    val dadosAnonimizados = informacaoSensivel.replace(padraoSensivel, "***")
    println(dadosAnonimizados)
}
```
Output:
```
Usuario: user***; Senha: pass***
```

## Deep Dive:
Desde os primórdios da programação, manipular strings tem sido uma tarefa comum. Linguagens antigas como Perl foram pioneiras em oferecer expressões regulares poderosas, que se tornaram ferramentas padrão em quase todas as linguagens modernas, incluindo Kotlin.

Alternativas para a remoção de caracteres incluem métodos de manipulação de strings como `filter` e `filterNot`, que permitem remover caracteres sem usar expressões regulares.

Detalhando a implementação, `replace` em Kotlin usa expressões regulares internamente para buscar padrões no texto e substituí-los. É um método flexível e robusto, mas se precisar de desempenho e a operação for simples, métodos baseados em caracteres podem às vezes ser mais rápidos.

## Veja Também:
- Documentação oficial do Kotlin sobre regex: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Kotlin `filter` e `filterNot`: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/filter.html](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/filter.html)
- Stack Overflow em Português: [https://pt.stackoverflow.com/](https://pt.stackoverflow.com/) - uma comunidade ativa onde você pode fazer perguntas e compartilhar conhecimento sobre a programação Kotlin e outros tópicos relacionados.
