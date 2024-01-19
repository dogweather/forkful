---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O quê e Por quê?
Converter uma string para minúsculas é uma operação que transforma todas as letras maiúsculas de uma string em letras minúsculas. Programadores fazem isso para padronizar os dados, facilitar comparações de string ou para garantir consistência ao trabalhar com textos sensíveis ao caso.

## Como fazer
Codigo de exemplo em Kotlin para converter uma string para minúsculas:

```Kotlin
val palavra = "Olá, Kotlin!"
val palavraMinuscula = palavra.lowercase()
println(palavraMinuscula)
```

A saída será:

```Kotlin
"olá, kotlin!"
```

## Mergulho Profundo
Na computação, a conversão de strings para minúsculas existe há muito tempo como um meio de facilitar a comparação de textos e a pesquisa. Kotlin, a partir da versão 1.5, mudou o método `toLowerCase` para `lowercase` para estar em conformidade com as convenções de nomenclatura. 

Existem alternativas para alcançar a mesma coisa em Kotlin, especialmente se você quiser considerar especificidades de localidade. Por exemplo, você pode usar o método `lowercase(Locale.getDefault())` para garantir que a conversão para minúsculas esteja de acordo com as regras dos idiomas locais.

A implementação deste método está na biblioteca-padrão de Kotlin e usa regras Unicode para realizar a conversão.

## Veja também
Para mais informações, consulte o link a seguir: 
- [Documentação oficial do Kotlin sobre Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- [Post do blog JetBrains sobre Kotlin 1.5](https://blog.jetbrains.com/kotlin/2021/02/new-language-features-preview-in-kotlin-1-5-0/)