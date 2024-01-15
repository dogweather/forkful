---
title:                "Convertendo uma string para minúsculas."
html_title:           "Kotlin: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que Converter uma String para Letras Minúsculas?

Há várias razões pelas quais pode ser necessário converter uma string para letras minúsculas em um programa Kotlin. Isso pode ser feito para garantir consistência no texto, facilitar a comparação de strings ou para atender a requisitos específicos do programa.

## Como Fazer Isso?

Existem algumas maneiras de converter uma string para letras minúsculas em Kotlin. Uma delas é usando o método `toLowerCase()` da classe String. Veja o exemplo abaixo:

```Kotlin
val nome = "JOÃO SILVA"
println(nome.toLowerCase())

// saída: joão silva 
```

Outra opção é utilizar a função de extensão `toLowerCase()`, que pode ser aplicada a qualquer string. Veja o exemplo:

```Kotlin
val sobrenome = "DA SILVA"
println(sobrenome.toLowerCase())

// saída: da silva
```

Você também pode usar o operador `?.` para lidar com strings nulas. Veja o exemplo:

```Kotlin
val nome: String? = null
println(nome?.toLowerCase())

// saída: null
```

## Profundando um Pouco Mais

Ao converter uma string para letras minúsculas em Kotlin, é importante notar algumas coisas. O método `toLowerCase()` e a função de extensão `toLowerCase()` não modifica a string original. Em vez disso, eles retornam uma nova string com as letras em minúsculo.

Também é importante lembrar que a conversão para minúsculas é sensível a localização. Isso significa que a saída pode ser diferente dependendo de qual idioma está sendo utilizado.

E por último, mas não menos importante, a função `toLowerCase()` utiliza as regras de conversão de texto do idioma padrão do dispositivo. Isso pode resultar em diferenças entre o comportamento em diferentes sistemas operacionais. Portanto, é sempre importante testar e verificar o comportamento em diferentes ambientes.

## Veja Também

- [Documentação do método `toLowerCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/to-lower-case.html)
- [Documentação da função de extensão `toLowerCase()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/text/to-lower-case.html)
- [Strings em Kotlin](https://kotlinlang.org/docs/strings.html)