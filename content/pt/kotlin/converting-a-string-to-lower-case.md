---
title:    "Kotlin: Convertendo uma string para minúsculas"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que converter uma string para minúsculas?

Ao trabalhar com strings em um programa Kotlin, é comum precisar convertê-las para minúsculas. Isso pode ser útil para realizar comparações de strings sem levar em conta a diferença entre letras maiúsculas e minúsculas, ou para garantir que os dados inseridos pelo usuário estejam todos em minúsculas para facilitar o processamento.

## Como fazer:

```Kotlin
val string = "EXEMPLO DE TEXTO EM MAIÚSCULAS"
val lowerCaseString = string.toLowerCase()
println(lowerCaseString)
```

Saída:

```
exemplo de texto em maiúsculas
```

Podemos utilizar o método `toLowerCase()` diretamente em uma string para convertê-la para minúsculas. Ele faz parte da classe `String` e não é necessário importar nenhuma biblioteca adicional.

## Aprofundando:

O método `toLowerCase()` é baseado no padrão Unicode, que define como os caracteres devem ser representados e manipulados em um programa de computador. Ele irá converter todas as letras maiúsculas da string para suas respectivas letras minúsculas, seguindo as regras estabelecidas pelo padrão Unicode.

É importante lembrar que esse método só irá converter as letras que fazem parte do alfabeto. Caracteres especiais, símbolos e números não serão afetados pela conversão.

Outra forma de converter uma string em minúsculas é utilizando a função `toLowercase()` da classe `CharSequence`. Dessa forma, podemos converter não apenas uma string, mas também outros tipos de dados que implementam a interface `CharSequence`, como `StringBuilder` e `StringBuffer`.

## Veja também:

- [Documentação oficial do método toLowerCase()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)
- [Documentação oficial da interface CharSequence](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-char-sequence/index.html)
- [Visão geral do padrão Unicode](https://unicode.org/standard/reports/tr10/#Case_Mappings)