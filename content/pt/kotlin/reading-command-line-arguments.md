---
title:    "Kotlin: Lendo argumentos da linha de comando"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por que ler argumentos de linha de comando?

Ler argumentos de linha de comando é uma habilidade importante para qualquer programador Kotlin, pois permite que você crie programas mais interativos e customizáveis. Além disso, compreender como os argumentos de linha de comando funcionam é fundamental para o desenvolvimento de aplicativos de linha de comando e scripts.

## Como fazer

Começaremos com o básico - o que são argumentos de linha de comando? Eles são informações que podem ser passadas para o seu programa Kotlin quando ele é executado a partir da linha de comando. Esses argumentos geralmente contêm dados ou opções que alteram o comportamento do programa. Para ler corretamente esses argumentos, precisamos usar a função `args` e especificar o tipo `Array<String>` como parâmetro. Veja um exemplo abaixo:

```Kotlin
fun main(args: Array<String>) {
    //seu código aqui
}
```

Agora, podemos acessar cada argumento usando a notação de índice `args[índice]`, sendo o índice 0 o primeiro argumento. Por exemplo, se quisermos imprimir o primeiro argumento na tela, poderíamos escrever o seguinte código:

```Kotlin
fun main(args: Array<String>) {
    println(args[0])
}
```

Caso você queira passar mais de um argumento, basta separá-los por espaços na linha de comando. O código abaixo mostra como imprimir o segundo e terceiro argumentos:

```Kotlin
fun main(args: Array<String>) {
    println(args[1])
    println(args[2])
}
```

Se você quiser que seu programa aceite um número variável de argumentos, pode utilizar o operador "spread" (`*`) antes do parâmetro `args`, como mostrado no exemplo abaixo:

```Kotlin
fun main(vararg args: String) {
    for (arg in args){
        println(arg)
    }
}
```

## Aprofundando

Agora que sabemos como acessar os argumentos de linha de comando, vamos entender melhor como eles funcionam. Ao executar um programa Kotlin a partir da linha de comando, o sistema operacional passa esses argumentos para o programa por meio da função `main`. O número de argumentos pode variar dependendo do que o usuário passou na linha de comando. Além disso, o usuário pode fornecer opções adicionais, como sinalizadores ou flags para modificar o comportamento do programa.

Para adicionar sinalizadores opcionais ao seu programa, você pode usar o pacote `kotlinx.cli`. Este pacote fornece uma maneira fácil e estruturada de ler e gerenciar argumentos de linha de comando. Para saber mais sobre como usar esse pacote, consulte a [documentação oficial](https://kotlin.github.io/kotlinx.cli/).

## Veja também

- [Documentação oficial do Kotlin para entrar mais a fundo no assunto](https://kotlinlang.org/docs/reference/command-line.html)
- [Exemplos práticos de uso de argumentos de linha de comando em Kotlin](https://medium.com/@abcoathup/kotlin-cli-4771d15c56e9)
- [Diferenças entre argumentos de linha de comando e opções da JVM](https://www.baeldung.com/kotlin/command-line-args-jvm-options)