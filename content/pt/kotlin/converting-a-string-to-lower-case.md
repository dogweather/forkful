---
title:                "Kotlin: Convertendo uma string para minúsculo"
simple_title:         "Convertendo uma string para minúsculo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Porque

Muitas vezes, ao trabalhar com strings em programas Kotlin, pode ser necessário converter uma string para letras minúsculas. Isso pode ser útil para padronizar a entrada, comparar strings com mais precisão ou simplesmente para fins estéticos. Felizmente, Kotlin oferece uma maneira fácil de fazer isso.

## Como Fazer

Para converter uma string para minúsculas em Kotlin, basta usar o método `toLowerCase ()` na string desejada. Por exemplo:

```Kotlin
var nome = "João"
var nomeMin = nome.toLowerCase()
print(nomeMin)
```
Isso irá imprimir "joão" no console. Como você pode ver, o método `toLowerCase ()` converteu todas as letras da string para minúsculas.

## Profundidade

É importante notar que o método `toLowerCase ()` não altera a string original, mas retorna uma nova string com todas as letras convertidas para minúsculas. Isso significa que você pode usar a string original sem se preocupar com possíveis mudanças inesperadas.

Outra coisa a notar é que o método `toLowerCase ()` usa a convenção de letras minúsculas do idioma padrão do sistema. Em outras palavras, se o seu sistema estiver configurado para usar português, todas as letras serão convertidas para minúsculas de acordo com as regras desse idioma.

Você também pode usar o método `toLowerCase (Locale)` para especificar manualmente o idioma que será usado na conversão. Por exemplo, se você quiser garantir que a string seja convertida para minúsculas de acordo com as regras do inglês, você pode usar o seguinte código:

```Kotlin
var nome = "João"
var nomeMin = nome.toLowerCase(Locale.ENGLISH)
print(nomeMin)
```
Isso resultará em "joão" sendo impresso no console, independentemente do idioma padrão do sistema.

## Veja Também

- Documentação oficial do Kotlin para mais informações sobre métodos de strings: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/
- Guia de estilo Kotlin para melhores práticas ao trabalhar com strings: https://kotlinlang.org/docs/tutorials/kotlin-for-py/guides/string-formatting.html.