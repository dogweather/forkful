---
title:                "Interpolando uma string"
html_title:           "Java: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolando Strings em Kotlin

## O Que & Porquê?

A interpolação de string é um recurso onde as variáveis são inseridas diretamente em strings. É útil para tornar o código mais compacto, legível e para otimizar a construção de strings.

## Como Fazer:

Aqui está um exemplo básico de interpolação de string em Kotlin:

```Kotlin
val nome = "João"
println("Olá, $nome!")
```
Resultado da execução:

```
Olá, João!
```
Podemos até mesmo inserir expressões mais complexas dentro desses marcadores:

```Kotlin
val idade = 30
println("No próximo ano você terá ${idade + 1} anos.")
```
Resultado da execução:

```
No próximo ano você terá 31 anos.
```

## Aprofundando

Historicamente, no Java e em outras linguagens semelhantes, você precisava usar a concatenação de string ou `String.format()` para conseguir algo semelhante à interpolação de string. Kotlin introduziu a interpolação de string para tornar a linguagem mais moderna e conveniente.

Os programadores costumavam usar o `StringBuilder` ou `StringBuffer` para construir strings em Java. O Kotlin se desvia desse método mais antigo em favor da interpolação de string.

Em termos de implementação, a interpolação de string em Kotlin é compilada em uma instância do `StringBuilder`. Portanto, essa sintaxe mais limpa não compromete a eficiência de sua execução.

## Veja Também

Para mais informações sobre Strings em Kotlin, consulte a documentação oficial [aqui](https://kotlinlang.org/docs/strings.html#string-literals).

Para um guia mais detalhado sobre interpolação de string e sua eficiência comparativa, consulte este [_post_ no blog](https://medium.com/androiddevelopers/strings-are-your-friends-1347e37bcae1) de Android Developers.