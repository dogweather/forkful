---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Kotlin: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O que & Porquê?
Deletar caracteres que correspondem a um determinado padrão é uma operação comum para programadores. Isso permite a manipulação eficiente de dados em uma string e é uma forma de resolver problemas de forma rápida e sem erros. 

## Como fazer:
Para deletar caracteres que correspondem a um determinado padrão em Kotlin, podemos usar a função ```replace(regex, replacement)```. Por exemplo, se quisermos deletar todos os caracteres que correspondem ao padrão "ABC", podemos usar o seguinte código:

```
val str = "ABCDABC"
val newStr = str.replace("ABC", "")
println(newStr) // output: D
```
Neste exemplo, a função ```replace()``` substitui todas as ocorrências do padrão "ABC" por uma string vazia, resultando na exclusão destes caracteres da string original.

## Profundidade:
Na programação, é comum encontrar situações em que precisamos manipular dados em strings. O processo de deletar caracteres que correspondem a um padrão pode ser feito de forma manual, mas isso pode ser tedioso e propenso a erros. Felizmente, linguagens de programação como Kotlin oferecem funções como a ```replace()``` que facilitam essa tarefa. Outra alternativa para a exclusão de caracteres é o uso de expressões regulares, que são sequências de caracteres que definem um padrão de busca. No caso de Kotlin, a função ```replace(regex, replacement)``` recebe uma expressão regular como parâmetro para a busca do padrão a ser excluído. Por fim, a implementação da função ```replace()``` em Kotlin é baseada no método ```replaceAll()``` da classe String do Java.

## Veja também:
- Documentação oficial do Kotlin para a função ```replace()```: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/kotlin.-kt-return-/replace.html
- Tutorial sobre expressões regulares em Kotlin: https://www.baeldung.com/kotlin-regular-expressions
- Perguntas frequentes sobre expressões regulares: https://code.tutsplus.com/pt/tutorials/8-regular-expressions-you-should-know--net-6149