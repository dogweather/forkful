---
title:                "Kotlin: Concatenando strings"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

A concatenação de strings é uma habilidade essencial para qualquer programador Kotlin. Ao combinarmos duas ou mais strings, podemos criar uma nova string personalizada e manipular nossos dados de forma eficiente. É uma técnica simples, mas poderosa, que pode ser usada em muitos cenários diferentes.

## Como fazer

Para concatenar strings em Kotlin, usamos o operador `+` ou o método `plus()`. Vamos dar uma olhada em alguns exemplos de código para entender melhor:

```Kotlin
// Exemplo de concatenação usando o operador +
val Saudacao = "Olá"
val nome = "Fulano"
val mensagem = saudacao + nome
println(mensagem) // Saída: Olá Fulano

// Exemplo de concatenação usando o método plus()
val texto = "Este é um texto"
val numero = "123"
val novoTexto = texto.plus(numero)
println(novoTexto) // Saída: Este é um texto123
```

Podemos usar o operador `+` para concatenar mais de duas strings. Também podemos usar o método `plus()` para adicionar um número ou outro tipo de dado à nossa string.

Agora, vamos dar uma olhada em um exemplo prático:

```Kotlin
val nome = "Maria"
val sobrenome = "Silva"
val idade = 25
val cidade = "São Paulo"
val endereco = nome + " " + sobrenome + " tem " + idade + " anos e mora em " + cidade
println(endereco) // Saída: Maria Silva tem 25 anos e mora em São Paulo
```

Nesse exemplo, criamos uma variável `endereco` que contém a concatenação de diferentes informações, criando uma frase descritiva sobre uma pessoa. Isso pode ser útil, por exemplo, em um programa que solicita informações de usuário e precisa exibir um texto personalizado.

## Deep Dive

Em Kotlin, podemos usar o operador `+=` para adicionar uma string a outra. Isso é equivalente a usar o operador `+` e atribuir o valor à mesma variável. Por exemplo:

```Kotlin
val texto = "Esta é uma"
texto += " string"
println(texto) // Saída: Esta é uma string
```

Além disso, quando concatenamos números com strings usando o operador `+`, o resultado será sempre uma string. Por exemplo:

```Kotlin
val idade = 30
val frase = "Eu tenho " + idade + " anos"
println(frase) // Saída: Eu tenho 30 anos
```

Esses são apenas alguns exemplos do que podemos fazer com a concatenação de strings em Kotlin. Lembre-se de que também podemos usar o método `format()` para criar strings formatadas, o que pode ser útil em algumas situações.

## Veja também

- [Documentação oficial do Kotlin sobre concatenação de strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Tutorial de Kotlin: Strings e caracteres](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)
- [Vídeo tutorial: Usando string templates em Kotlin](https://www.youtube.com/watch?v=7m-_e-yJSA4)