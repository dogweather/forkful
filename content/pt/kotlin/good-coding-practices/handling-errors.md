---
date: 2024-01-26 00:54:21.325281-07:00
description: "Tratar erros \xE9 a forma como o seu c\xF3digo lida com problemas que\
  \ surgem durante a execu\xE7\xE3o\u2014como lidar com uma bola curva sem deix\xE1\
  -la cair. Os\u2026"
lastmod: '2024-03-13T22:44:46.549721-06:00'
model: gpt-4-1106-preview
summary: "Tratar erros \xE9 a forma como o seu c\xF3digo lida com problemas que surgem\
  \ durante a execu\xE7\xE3o\u2014como lidar com uma bola curva sem deix\xE1-la cair."
title: Tratamento de erros
weight: 16
---

## O Que & Porquê?
Tratar erros é a forma como o seu código lida com problemas que surgem durante a execução—como lidar com uma bola curva sem deixá-la cair. Os programadores fazem isso para evitar falhas e proporcionar aos usuários uma experiência suave.

## Como fazer:
Kotlin fornece `try`, `catch`, `finally`, e `throw` para gerenciar erros. Veja como usá-los:

```Kotlin
fun main() {
    val numerador = 10
    val denominador = 0

    try {
        val resultado = numerador / denominador
        println("Resultado: $resultado")
    } catch (e: ArithmeticException) {
        println("Não é possível dividir por zero, amigo.")
    } finally {
        println("Isso acontece independente do que ocorrer.")
    }
}
```

Saída:
```
Não é possível dividir por zero, amigo.
Isso acontece independente do que ocorrer.
```

Se algo der errado no bloco `try`, a execução salta para o `catch`. Ele captura o erro específico lançado (`ArithmeticException` neste caso). O bloco `finally` é executado depois—não importa o resultado.

## Mergulho Profundo
O bloco `try-catch` existe desde os primeiros dias de programação—é como uma rede de segurança. Kotlin também oferece `throw` para lançar manualmente uma exceção na arena, e há `finally` para código que deve ser executado—trabalho de limpeza, frequentemente.

Alternativas incluem o tipo `Result` e o `try` de Kotlin como uma expressão.

```Kotlin
val resultado: Result<Int> = try {
    Result.success(numerador / denominador)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Essa abordagem retorna um objeto `Result`—você obtém um sucesso ou uma falha sem o drama de uma exceção não tratada.

A implementação em Kotlin é elegante porque você pode usar `try` como uma expressão, o que significa que ele retorna um valor. Opções como essas tornam o tratamento de erros em Kotlin bastante versátil. É sobre escolher a ferramenta certa para o trabalho, assim como você faria em uma oficina.

## Veja Também
- Documentação de Kotlin sobre Exceções: [Tratamento de Exceção em Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Documentação do tipo `Result` de Kotlin: [Resultado Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3ª Edição, de Joshua Bloch—ótimas percepções sobre exceções, apesar de ser específico para Java.
