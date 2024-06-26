---
date: 2024-01-20 17:51:04.199468-07:00
description: "Como Fazer: Interpola\xE7\xE3o de strings n\xE3o \xE9 exclusiva de Kotlin\
  \ e existe em v\xE1rias linguagens modernas. Historicamente, construir strings din\xE2\
  micas envolvia\u2026"
lastmod: '2024-04-05T22:50:59.786719-06:00'
model: gpt-4-1106-preview
summary: "Interpola\xE7\xE3o de strings n\xE3o \xE9 exclusiva de Kotlin e existe em\
  \ v\xE1rias linguagens modernas."
title: Interpolando uma string
weight: 8
---

## Como Fazer:
```Kotlin
fun main() {
    val nome = "João"
    val idade = 28
    println("Meu nome é $nome e eu tenho $idade anos.")
    println("Ano que vem, farei ${idade + 1} anos.")
}
```
**Saída:**
```
Meu nome é João e eu tenho 28 anos.
Ano que vem, farei 29 anos.
```

## Mergulho Profundo
Interpolação de strings não é exclusiva de Kotlin e existe em várias linguagens modernas. Historicamente, construir strings dinâmicas envolvia a concatenação, que pode se tornar verbosa e propensa a erros. No Kotlin, a interpolação é feita usando o símbolo `$` seguido pelo nome da variável ou dentro de `${}` para expressões. Em termos de performance, o Kotlin compila interpolação de string para uma `StringBuilder` sob o capô, o que é eficiente. Alternativas incluem a concatenação manual (menos legível) ou o uso de templates externos como Apache Velocity.

## Veja Também
- Documentação oficial Kotlin sobre strings: [kotlinlang.org](https://kotlinlang.org/docs/reference/basic-types.html#string-templates)
- Guia Kotlin sobre `StringBuilder`: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- Comparação entre interpolação e concatenação: [medium.com](https://medium.com/@vanniktech/string-concatenation-with-kotlin-and-java-8-71f4b3e5098e)
