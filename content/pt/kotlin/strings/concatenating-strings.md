---
date: 2024-01-20 17:35:27.639253-07:00
description: "Como Fazer: Concatenar strings n\xE3o \xE9 nada novo na programa\xE7\
  \xE3o; \xE9 um conceito t\xE3o antigo quanto as pr\xF3prias linguagens de programa\xE7\
  \xE3o. Em Kotlin, a\u2026"
lastmod: '2024-04-05T21:53:46.875648-06:00'
model: gpt-4-1106-preview
summary: "Concatenar strings n\xE3o \xE9 nada novo na programa\xE7\xE3o; \xE9 um conceito\
  \ t\xE3o antigo quanto as pr\xF3prias linguagens de programa\xE7\xE3o."
title: Concatenando strings
weight: 3
---

## Como Fazer:
```kotlin
fun main() {
    val cumprimento = "Olá"
    val nome = "João"
    
    // Uso do operador '+'
    val mensagem = cumprimento + ", " + nome + "!"
    println(mensagem) // Saída: Olá, João!
    
    // Interpolação de String com '$'
    val mensagemInterpolada = "$cumprimento, $nome!"
    println(mensagemInterpolada) // Saída: Olá, João!
    
    // Usando a função 'concat'
    val cumprimentoCompleto = cumprimento.concat(", ").concat(nome).concat("!")
    println(cumprimentoCompleto) // Saída: Olá, João!
}
```

## Aprofundando
Concatenar strings não é nada novo na programação; é um conceito tão antigo quanto as próprias linguagens de programação. Em Kotlin, a concatenação se dá de maneira intuitiva. Além do operador `+` e da interpolação usando `$`, Kotlin oferece métodos como `concat` ou `plus`. Há outras alternativas, como o uso de `StringBuilder` para concatenações em loops ou em cenários com alta performance, pois ele minimiza a criação de objetos intermediários.

Kotlin é uma linguagem de programação moderna que roda na JVM (Java Virtual Machine), mas também compila para JavaScript ou Native. Isso significa que algumas técnicas de otimização que aplicam para Java também valem para Kotlin, especialmente no backend ou em aplicativos Android.

## Veja Também
- Documentação oficial do Kotlin sobre strings: [Strings - Kotlin Programming Language](https://kotlinlang.org/docs/strings.html)
- Video explicativo sobre interpolação de strings em Kotlin: [Kotlin - String Interpolation](https://www.youtube.com/watch?v=SmPouefk7pI)
