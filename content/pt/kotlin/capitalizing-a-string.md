---
title:                "Kotlin: Transformando uma string em maiúscula"
simple_title:         "Transformando uma string em maiúscula"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que
Capitalizar uma string é um processo comum no desenvolvimento de aplicativos para garantir que a aparência do texto seja consistente e legível. É uma prática importante em qualquer linguagem de programação, incluindo Kotlin.

## Como Fazer
Para capitalizar uma string em Kotlin, podemos usar o método `capitalize()` da classe `String`. Este método transforma o primeiro caractere da string em maiúscula e mantém o restante do texto inalterado. Vamos ver um exemplo abaixo:

```Kotlin
val texto = "olá, mundo!"
println(texto.capitalize())
```
A saída deste código será "Olá, mundo!".

Para capitalizar uma string inteira, podemos usar o método `split()` para dividir a string em várias palavras e, em seguida, aplicar o método `capitalize()` a cada uma delas. Veja um exemplo:

```Kotlin
val texto = "capitalizar cada palavra desta string"
val palavras = texto.split(" ")
val resultado = palavras.joinToString(" ") {it.capitalize()}
println(resultado)
```
A saída será "Capitalizar Cada Palavra Desta String".

## Aprofundando
Ao capitalizar uma string, é importante considerar o idioma em que o texto está escrito. Por exemplo, em português, a letra "ç" deve ser maiúscula como "Ç". No entanto, o método `capitalize()` não reconhece isso e pode causar problemas de formatação em palavras com "ç".

Uma solução para esse problema é usar o método `toUpperCase()` junto com o método `substring()`, que deixa a primeira letra como maiúscula enquanto mantém o restante da string inalterado. Veja um exemplo abaixo:

```Kotlin
val texto = "maçã"
val resultado = texto[0].toString().toUpperCase() + texto.substring(1)
println(resultado)
```
A saída será "Maçã".

## Veja Também
- [Documentação oficial do método `capitalize()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Tutorial sobre strings em Kotlin](https://www.devmedia.com.br/trabalhando-com-strings-em-kotlin/34058)
- [Official Kotlin Documentation](https://kotlinlang.org/docs/home.html)