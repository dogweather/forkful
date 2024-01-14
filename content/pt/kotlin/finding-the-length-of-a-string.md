---
title:    "Kotlin: Encontrando o comprimento de uma string."
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string pode parecer uma tarefa simples, mas é um conceito fundamental em programação. Ao entender como encontrar o comprimento de uma string, você pode criar códigos mais eficientes e precisos ao lidar com dados de texto.

## Como fazer

Para encontrar o comprimento de uma string em Kotlin, podemos usar o método `length`, que é acessado através do operador de ponto (`.`). Veja um exemplo abaixo:

```Kotlin
val string = "Olá, mundo!"
println(string.length)
```

Neste exemplo, definimos uma variável `string` que armazena a string "Olá, mundo!". Em seguida, usamos o operador de ponto para acessar o método `length` e imprimir o resultado. O output deste código será `12`, pois a string contém 12 caracteres.

Também podemos usar o método `length` em uma string vazia, como mostrado abaixo:

```Kotlin
val stringVazia = ""
println(stringVazia.length)
```

Neste caso, o output será `0`, pois uma string vazia não contém nenhum caractere.

## Deep Dive

Para entender melhor como o método `length` funciona, precisamos entender alguns conceitos de string em Kotlin. Em Kotlin, uma string é uma sequência de caracteres que é armazenada como um objeto. Quando usamos o método `length`, ele retorna o número de caracteres na string, que inclui espaços em branco e símbolos especiais.

Além disso, é importante ressaltar que o método `length` sempre retorna um valor do tipo `Int`, ou seja, um número inteiro. Portanto, se tentarmos atribuir o resultado do método `length` a uma variável do tipo `String`, recebemos um erro de tipo.

## Veja Também

- Documentação oficial do Kotlin sobre o método `length`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/length.html
- Artigo sobre strings em Kotlin: https://kotlinexpertise.com/kotlin-strings/
- Vídeo tutorial sobre o uso do método `length`: https://www.youtube.com/watch?v=adS1IHZPEqA