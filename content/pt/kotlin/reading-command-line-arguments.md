---
title:    "Kotlin: Lendo argumentos da linha de comando"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Por que ler argumentos de linha de comando em Kotlin

Muitos programas e aplicativos exigem que os usuários forneçam informações personalizadas ou variáveis durante a execução. Isso pode ser feito de várias maneiras, mas uma das mais comuns é através de argumentos de linha de comando. Neste post, vamos explorar o porquê de ser importante saber como ler e trabalhar com esses argumentos em Kotlin.

Como ler argumentos de linha de comando em Kotlin

Para ler argumentos de linha de comando em Kotlin, precisamos usar o objeto `args` dentro da função `main()`. Vamos ver um exemplo de código:

```Kotlin
fun main(args: Array<String>) {
    println("O primeiro argumento é ${args[0]}")
}
```

Assumindo que o usuário inseriu `kotlin` como primeiro argumento, o programa imprimirá "O primeiro argumento é kotlin".

Também podemos usar as funções de extensão `forEach()` e `withIndex()` para percorrer todos os argumentos de linha de comando:

```Kotlin
fun main(args: Array<String>) {
    args.forEach { println(it) }
    args.withIndex().forEach { println("$it: ${args[it.index]}") }
}
```

Isso imprimirá cada argumento individualmente e também sua posição no array.

Deep Dive em argumentos de linha de comando

Além de ler os argumentos de linha de comando, podemos manipulá-los e validar seu conteúdo. Por exemplo, podemos verificar o tamanho do array `args` para garantir que todos os argumentos necessários foram fornecidos pelo usuário. Também podemos usar expressões regulares para verificar se os argumentos seguem um determinado padrão.

Também é importante saber que os argumentos de linha de comando são passados como `Strings`, portanto, podemos usá-los diretamente em expressões e funções de string.

Veja também

- [Documentação oficial do Kotlin sobre argumentos de linha de comando](https://kotlinlang.org/docs/command-line.html)
- [Tutorial do Mkyong sobre argumentos de linha de comando em Kotlin](https://mkyong.com/kotlin/kotlin-read-command-line-argument/)
- [Vídeo tutorial do canal Kotlin Guy sobre argumentos de linha de comando no Kotlin](https://www.youtube.com/watch?v=3DkloKtoLrM)

Esperamos que este post tenha te ajudado a entender melhor como ler e trabalhar com argumentos de linha de comando em Kotlin. Com essa habilidade, você pode criar programas mais interativos e personalizados para seus usuários. Para mais informações e dicas sobre programação em Kotlin, fique de olho em nosso blog e nos siga nas redes sociais. Até a próxima!
# Veja também.