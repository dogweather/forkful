---
title:    "Kotlin: Imprimindo saída de depuração"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração é importante

Imprimir resultados de depuração é uma maneira útil de ver o que está acontecendo em seu código enquanto ele é executado. Isso pode ajudá-lo a encontrar erros e resolver problemas mais facilmente.

## Como fazer

Para imprimir uma saída de depuração em Kotlin, você pode usar o método `println()` ou `Log.d()`.

```Kotlin
val nome = "João"
val idade = 25
println("$nome tem $idade anos.")

// Saída: João tem 25 anos.
```

Você também pode imprimir variáveis ​​e expressões para obter mais informações:

```Kotlin
val a = 10
val b = 5
val soma = a + b
println("A soma de $a e $b é igual a $soma.")

// Saída: A soma de 10 e 5 é igual a 15.
```

Se você estiver depurando um aplicativo Android, pode usar o `Log.d()` para imprimir saídas diretamente no logcat:

```Kotlin
val texto = "Isso é um teste"
Log.d("DEBUG", texto)

// Saída no logcat: DEBUG: Isso é um teste
```

## Mergulho Profundo

Além de imprimir valores, você também pode imprimir mensagens de erro e exceções para ajudá-lo a encontrar e corrigir problemas em seu código. Você pode usar o `Log.e()` para imprimir uma mensagem de erro e o `Log.w()` para imprimir um aviso.

```Kotlin
val numero = "123"
try {
    val conversao = numero.toInt()
    println("O número é $conversao.")
} catch(e: NumberFormatException) {
    Log.e("ERRO", e.message)
}

// Saída: ERRO: For input string: "123"
```

Outra maneira de imprimir informações de depuração é usar o recurso de log da linha de comando do Kotlin. Isso permite que você imprima mensagens de depuração sem alterar seu código. Basta adicionar a opção `-d` ao compilar o programa e usar o método `println()` para imprimir as saídas.

```sh
kotlinc -d Hello.kt -include-runtime -d hello.jar
java -jar hello.jar

Criação de saída de depuração: Dados Criados
```

## Veja também

- [Documentação oficial do Kotlin](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Depuração de aplicativos Android com Android Studio](https://developer.android.com/studio/debug/)